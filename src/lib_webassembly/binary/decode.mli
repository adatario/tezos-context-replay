module Vector = Lazy_vector.LwtInt32Vector

exception Code of Source.region * string

(** Lazy stack using an underlying lazy vector, and a pointer on the head of the
    stack. *)
type 'a lazy_stack = LazyStack of {length : int32; vector : 'a Vector.t}

(** [empty_stack ()] returns a new empty stack. *)
val empty_stack : unit -> 'a lazy_stack

(** [push_stack v s] pushes value [v] on the stack [s] at index [s.length] and
    updates the length. If [s.vector] is full, it appends the value to the vector
    (growing it by 1), otherwise it set the value in the vector. *)
val push_stack : 'a -> 'a lazy_stack -> 'a lazy_stack

(** [push_rev_values vs s] pushes values in [vs] on the stack [s], starting from
    the end of the list. *)
val push_rev_values : 'a list -> 'a lazy_stack -> 'a lazy_stack

(** [pop_stack s] returns the value at [s.length - 1] and a new stack with
    length decreased by one. *)
val pop_stack : 'a lazy_stack -> ('a * 'a lazy_stack) option Lwt.t

(** [pop_at_most n s] returns at most the [n] values on top of the stack, and
    the resulting stack. *)
val pop_at_most : int -> 'a lazy_stack -> ('a list * 'a lazy_stack) Lwt.t

(** Instruction parsing continuations. *)
type instr_block_kont =
  | IKStop of Ast.block_label  (** Final step of a block parsing. *)
  | IKNext of Ast.block_label
      (** Tag parsing, containing the accumulation of already parsed values. *)
  | IKBlock of Ast.block_type * int  (** Block parsing step. *)
  | IKLoop of Ast.block_type * int  (** Loop parsing step. *)
  | IKIf1 of Ast.block_type * int  (** If parsing step. *)
  | IKIf2 of Ast.block_type * int * Ast.block_label
      (** If .. else parsing step. *)

(** Block parsing continuations. *)
type block_kont =
  | BlockStart
      (** Initial step of a block parsing, allocating the block in the block table. *)
  | BlockParse of instr_block_kont lazy_stack
      (** Parsing of a block, with the continuation stack. *)
  | BlockStop of Ast.block_label
      (** End of a block, returning the label corresponding to the allocated block
          at the beginning. *)

(** Vector and size continuations *)

(** Lazy vector accumulator, with the current offset to write the next value in
    the vector. *)
type 'a lazy_vec_kont = LazyVec of {offset : int32; vector : 'a Vector.t}

(** Position of a value on the stream. *)
type pos = private int

(** Size checking version of {!sized} for CPS-style parsing. *)
type size = {size : int; start : pos}

(** Incremental chunked byte vector creation (from implicit input). *)
type byte_vector_kont =
  | VKStart  (** Initial step. *)
  | VKRead of Chunked_byte_vector.Lwt.t * int64 * int64
      (** Reading step, containing the current position in the string and the
      length, reading byte per byte. *)
  | VKStop of Chunked_byte_vector.Lwt.t  (** Final step, cannot reduce. *)

type name_step =
  | NKStart  (** UTF8 name starting point. *)
  | NKParse of pos * int lazy_vec_kont * int  (** UTF8 char parsing. *)
  | NKStop of Ast.name  (** UTF8 name final step.*)

type func_type_kont =
  | FKStart
  | FKIns of Types.value_type lazy_vec_kont
  | FKOut of Types.value_type Vector.t * Types.value_type lazy_vec_kont
  | FKStop of Types.func_type

type import_kont =
  | ImpKStart  (** Import parsing starting point. *)
  | ImpKModuleName of name_step
      (** Import module name parsing UTF8 char per char step. *)
  | ImpKItemName of Ast.name * name_step
      (** Import item name parsing UTF8 char per char step. *)
  | ImpKStop of Ast.import'  (** Import final step. *)

type export_kont =
  | ExpKStart  (** Export parsing starting point. *)
  | ExpKName of name_step  (** Export name parsing UTF8 char per char step. *)
  | ExpKStop of Ast.export'  (** Export final step. *)

(** Code section parsing. *)
type code_kont =
  | CKStart  (** Starting point of a function parsing. *)
  | CKLocalsParse of {
      left : pos;
      size : size;
      pos : pos;
      vec_kont : (int32 * Types.value_type) lazy_vec_kont;
      locals_size : Int64.t;
    }  (** Parse a local value with its number of occurences. *)
  | CKLocalsAccumulate of {
      left : pos;
      size : size;
      pos : pos;
      type_vec : (int32 * Types.value_type) lazy_vec_kont;
      curr_type : (int32 * Types.value_type) option;
      vec_kont : Types.value_type lazy_vec_kont;
    }  (** Accumulate local values. *)
  | CKBody of {
      left : pos;
      size : size;
      locals : Types.value_type Vector.t;
      const_kont : block_kont;
    }  (** Parsing step of the body of a function. *)
  | CKStop of Ast.func  (** Final step of a parsed function, irreducible. *)

type index_kind = Indexed | Const

type elem_kont =
  | EKStart  (** Starting point of an element segment parsing. *)
  | EKMode of {
      left : pos;
      index : int32 Source.phrase;
      index_kind : index_kind;
      early_ref_type : Types.ref_type option;
      offset_kont : pos * block_kont;
    }  (** Element segment mode parsing step. *)
  | EKInitIndexed of {
      mode : Ast.segment_mode;
      ref_type : Types.ref_type;
      einit_vec : Ast.const lazy_vec_kont;
    }
      (** Element segment initialization code parsing step for referenced values. *)
  | EKInitConst of {
      mode : Ast.segment_mode;
      ref_type : Types.ref_type;
      einit_vec : Ast.const lazy_vec_kont;
      einit_kont : pos * block_kont;
    }
      (** Element segment initialization code parsing step for constant values. *)
  | EKStop of Ast.elem_segment'  (** Final step of a segment parsing. *)

type data_kont =
  | DKStart  (** Starting point of a data segment parsing. *)
  | DKMode of {
      left : pos;
      index : int32 Source.phrase;
      offset_kont : pos * block_kont;
    }  (** Data segment mode parsing step. *)
  | DKInit of {dmode : Ast.segment_mode; init_kont : byte_vector_kont}
  | DKStop of Ast.data_segment'  (** Final step of a data segment parsing. *)

(** Representation of a section tag. *)
type section_tag =
  [ `CodeSection
  | `CustomSection
  | `DataCountSection
  | `DataSection
  | `ElemSection
  | `ExportSection
  | `FuncSection
  | `GlobalSection
  | `ImportSection
  | `MemorySection
  | `StartSection
  | `TableSection
  | `TypeSection ]

(** Type witness for a LazyVector. *)
type vec_repr = VecRepr

(** Type witness for an option. *)
type opt_repr = OptRepr

(** Sections representation. *)
type (_, _) field_type =
  | TypeField : (Ast.type_, vec_repr) field_type
  | ImportField : (Ast.import, vec_repr) field_type
  | FuncField : (Ast.var, vec_repr) field_type
  | TableField : (Ast.table, vec_repr) field_type
  | MemoryField : (Ast.memory, vec_repr) field_type
  | GlobalField : (Ast.global, vec_repr) field_type
  | ExportField : (Ast.export, vec_repr) field_type
  | StartField : (Ast.start, opt_repr) field_type
  | ElemField : (Ast.elem_segment, vec_repr) field_type
  | DataCountField : (int32, opt_repr) field_type
  | CodeField : (Ast.func, vec_repr) field_type
  | DataField : (Ast.data_segment, vec_repr) field_type

(** Result of a section parsing, being either a single value or a vector. *)
type field =
  | VecField : ('a, vec_repr) field_type * 'a Vector.t -> field
  | SingleField : ('a, opt_repr) field_type * 'a option -> field

(** Module parsing steps *)
type module_kont =
  | MKStart  (** Initial state of a module parsing *)
  | MKSkipCustom : (('a, 'repr) field_type * section_tag) option -> module_kont
      (** Custom section which are skipped, with the next section to parse. *)
  | MKFieldStart : ('a, 'repr) field_type * section_tag -> module_kont
      (** Starting point of a section, handles parsing generic section header. *)
  | MKField : ('a, 'repr) field_type * size * 'a lazy_vec_kont -> module_kont
      (** Section currently parsed, accumulating each element from the underlying vector. *)
  | MKElaborateFunc :
      Ast.var Vector.t * Ast.func Vector.t * Ast.func lazy_vec_kont * bool
      -> module_kont
      (** Elaboration of functions from the code section with their declared type in
      the func section, and accumulating invariants conditions associated to
      functions. *)
  | MKBuild of Ast.func Vector.t option * bool
      (** Accumulating the parsed sections vectors into a module and checking
      invariants. *)
  | MKStop of Ast.module_' (* TODO (#3120): actually, should be module_ *)
      (** Final step of the parsing, cannot reduce. *)
  | MKTypes of func_type_kont * pos * size * Ast.type_ lazy_vec_kont
      (** Function types section parsing. *)
  | MKImport of import_kont * pos * size * Ast.import lazy_vec_kont
      (** Import section parsing. *)
  | MKExport of export_kont * pos * size * Ast.export lazy_vec_kont
      (** Export section parsing. *)
  | MKGlobal of
      Types.global_type * int * block_kont * size * Ast.global lazy_vec_kont
      (** Globals section parsing, containing the starting position, the
      continuation of the current global block instruction, and the size of the
      section. *)
  | MKElem of elem_kont * int * size * Ast.elem_segment lazy_vec_kont
      (** Element segments section parsing, containing the current element parsing
      continuation, the starting position of the current element, the size of
      the section. *)
  | MKData of data_kont * int * size * Ast.data_segment lazy_vec_kont
      (** Data segments section parsing, containing the current data parsing
      continuation, the starting position of the current data, the size of the
      section. *)
  | MKCode of code_kont * int * size * Ast.func lazy_vec_kont
      (** Code section parsing, containing the current function parsing
      continuation, the starting position of the current function, the size of
      the section. *)

(** Parsed bytes with the current reading position. *)
type stream = {name : string; bytes : string; pos : pos ref}

(** Allocation state of basic blocks. The type relies on the mutable state
    internaly, but making it private avoids being able to modify it externally.
*)
type block_state = private {mutable new_blocks : Ast.instr Vector.t Vector.t}

(** Accumulator of parsed fields *)
type building_state = {
  types : Ast.type_ Vector.t;
  imports : Ast.import Vector.t;
  vars : Ast.var Vector.t;
  tables : Ast.table Vector.t;
  memories : Ast.memory Vector.t;
  globals : Ast.global Vector.t;
  exports : Ast.export Vector.t;
  start : Ast.start option;
  elems : Ast.elem_segment Vector.t;
  data_count : int32 option;
  code : Ast.func Vector.t;
  datas : Ast.data_segment Vector.t;
}

(** Decoding continuation step. *)
type decode_kont = {
  building_state : building_state;
      (** Accumulated parsed sections, used to build the final module. *)
  module_kont : module_kont;  (** Module continuation. *)
  stream : stream;  (** Parsed stream. *)
  block_state : block_state;  (** Basic blocks allocated. *)
}

(** [make_stream filename bytes] returns a new stream to decode. *)
val make_stream : name:string -> bytes:string -> stream

(** [make_empty_block_state ()] returns a new block allocation state. *)
val make_empty_block_state : unit -> block_state

(** [make_block_state blocks] returns a new block allocation state from already
    existing blocks allocation. This function is used to build block state during
    tree decoding. *)
val make_block_state : Ast.instr Vector.t Vector.t -> block_state

(** [module_step kont] takes one step of parsing from a continuation and returns
   a new continuation. Fails when the contination of the module is [MKStop]
   since it cannot reduce. *)
val module_step : decode_kont -> decode_kont Lwt.t

(** [decode ~name ~bytes] decodes a module [name] from its [bytes] encoding.

    @raise Code on parsing errors. *)
val decode : name:string -> bytes:string -> Ast.module_ Lwt.t

(** [decode ~name ~bytes] decodes a custom section of name [name] from its
    [bytes] encoding.

    @raise Code on parsing errors. *)
val decode_custom : Ast.name -> name:string -> bytes:string -> string list

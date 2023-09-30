(*

Put all helper functions needed for keywordcount here. This should contain no functions
that help only your implementation of simpledict. Helper functions should be tested, and
they go in this library so that they are testable. If they are only in keywordcount.ml,
which compiles to an executable, then they are not accessible from a test file.

Feel free to remove `placeholder` when you add your actual functions.
   
*)

val traverse_directory : string -> string list

val read_file_as_string : string -> string 

val filter_strings : string -> string

val filter_comments : string -> string

val filter_non_characters : string-> string

val count_kw_occurance : kw_list:string list -> path:string -> int Simpledict.t

val sort_kw_list_by_value : int Simpledict.t -> (string * int) list

val list_to_sexp : (string * int) list -> string

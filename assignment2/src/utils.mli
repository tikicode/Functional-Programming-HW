(*

Put all helper functions needed for keywordcount here. This should contain no functions
that help only your implementation of simpledict. Helper functions should be tested, and
they go in this library so that they are testable. If they are only in keywordcount.ml,
which compiles to an executable, then they are not accessible from a test file.

Feel free to remove `placeholder` when you add your actual functions.
   
*)

val traverse_directory : string -> string list
(** Traverse a directory and file all ".ml" and ".mli" files given an input path. 
    If the path is invalid, rather than throwing, an error, traverse the current directory.
    Return a list of strings containing the full path of all OCaml files in the directory,
    including in subdirectories. *)

val read_file_as_string : string -> string
(** Return a string containing the contents of a file from a path to the file. *)

val filter_strings : string -> string
(** Remove all strings denoted by an opening and closing quote character from a
    given string containing the contents of an OCaml file. *)

val filter_comments : string -> string
(** Remove all OCaml style comments, including nested and multiline comments, from
    a given input string. *)

val filter_non_characters : string -> string
(** Remove all non-alphanumeric and _ characters from a given input string, replacing 
    them with a space *)

val count_kw_occurance : kw_list:string list -> path:string -> int Simpledict.t
(** Count the occurance of keywords in a file given a list of keywords and a path to 
    the file to parse for keywords. In order to properly count keywords, all strings, 
    comments, and non-characters must be removed the from the file. Return a dictionary
    containing the keywords and their counts. *)

val sort_kw_list_by_value : int Simpledict.t -> (string * int) list
(** Given a dictionary of keywords and their counts, convert the dictionary to a list
    and sort the keywords by value so the most frequently occuring keywords and their
    counts appear first. If counts of two or more keywords are equal, ensure they are 
    printed in alphabetical order. *)

val list_to_sexp : (string * int) list -> string
(** Return an s-expression of a list of keywords and their counts. *)

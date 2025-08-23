; Keywords
[
  "let"
  "fn"
  "return"
  "for"
  "break"
  "continue"
  "if"
  "else"
  "true"
  "false"
] @keyword

; Built-in functions
(builtin_function) @function.builtin

; String literals
(string_literal) @string

; Comments
(comment) @comment

; Numbers
(integer_literal) @number
(float_literal) @number

; Boolean and null literals
(boolean_literal) @constant.builtin
(null_literal) @constant.builtin

; Function definitions
(function_statement
  name: (identifier) @function)

; Function calls
(function_call
  function: (identifier) @function.call)

; Parameters
(parameter_list
  (identifier) @parameter)

; Let variables
(let_statement
  name: (identifier) @variable.definition)

; Variable references - match only identifiers in primary_expression
(primary_expression
  (identifier) @variable)

; Operators
[
  "="
  "+="
  "-="
  "*="
  "/="
  "%="
  "&="
  "|="
  "^="
  "<<="
  ">>="
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "and"
  "or"
  "&"
  "|"
  "^"
  "<<"
  ">>"
  "!"
] @operator

; Punctuation
[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
  ";"
  ","
  ":"
] @punctuation

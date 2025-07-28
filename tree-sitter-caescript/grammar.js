module.exports = grammar({
  name: 'caescript',

  extras: $ => [
      /\s/, // whitespace
      $.comment,
  ],

  rules: {
      // Start symbol
      program: $ => repeat($.statement),

      // Comments
      comment: $ => token(seq('//', /.*/)),

      // Statements
      statement: $ => choice(
          $.let_statement,
          $.return_statement,
          $.for_statement,
          $.break_statement,
          $.continue_statement,
          $.function_statement,
          $.expression_statement,
      ),

      // Let statement: let identifier = expression;
      let_statement: $ => seq(
          'let',
          field('name', $.identifier),
          '=',
          field('value', $.expression),
          optional(';')
      ),

      // Return statement: return [expression];
      return_statement: $ => prec.right(seq(
          'return',
          optional(field('value', $.expression)),
          optional(';')
      )),

      // For statement: for (condition) { body }
      for_statement: $ => seq(
          'for',
          '(',
          field('condition', $.expression),
          ')',
          field('body', $.block_statement)
      ),

      // Break statement
      break_statement: $ => seq('break', optional(';')),

      // Continue statement
      continue_statement: $ => seq('continue', optional(';')),

      // Expression statement: expression;
      expression_statement: $ => seq($.expression, optional(';')),

      // Block statement: { statements }
      block_statement: $ => seq(
          '{',
          repeat($.statement),
          '}'
      ),

      // Expressions
      expression: $ => choice(
          $.assignment_expression,
          $.logical_or_expression,
      ),

      // Assignment expressions: identifier = expression, identifier += expression, etc.
      assignment_expression: $ => prec.right(1, seq(
          field('left', $.identifier),
          field('operator', choice('=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=')),
          field('right', $.expression)
      )),

      // Logical OR: expression || expression
      logical_or_expression: $ => prec.left(2, choice(
          $.logical_and_expression,
          seq(
              field('left', $.logical_or_expression),
              field('operator', choice('||', 'or')),
              field('right', $.logical_and_expression)
          )
      )),

      // Logical AND: expression && expression
      logical_and_expression: $ => prec.left(3, choice(
          $.bitwise_or_expression,
          seq(
              field('left', $.logical_and_expression),
              field('operator', choice('&&', 'and')),
              field('right', $.bitwise_or_expression)
          )
      )),

      // Bitwise OR: expression | expression
      bitwise_or_expression: $ => prec.left(4, choice(
          $.bitwise_xor_expression,
          seq(
              field('left', $.bitwise_or_expression),
              field('operator', '|'),
              field('right', $.bitwise_xor_expression)
          )
      )),

      // Bitwise XOR: expression ^ expression
      bitwise_xor_expression: $ => prec.left(5, choice(
          $.bitwise_and_expression,
          seq(
              field('left', $.bitwise_xor_expression),
              field('operator', '^'),
              field('right', $.bitwise_and_expression)
          )
      )),

      // Bitwise AND: expression & expression
      bitwise_and_expression: $ => prec.left(6, choice(
          $.equality_expression,
          seq(
              field('left', $.bitwise_and_expression),
              field('operator', '&'),
              field('right', $.equality_expression)
          )
      )),

      // Equality: expression == expression, expression != expression
      equality_expression: $ => prec.left(7, choice(
          $.relational_expression,
          seq(
              field('left', $.equality_expression),
              field('operator', choice('==', '!=')),
              field('right', $.relational_expression)
          )
      )),

      // Relational: expression < expression, expression <= expression, etc.
      relational_expression: $ => prec.left(8, choice(
          $.shift_expression,
          seq(
              field('left', $.relational_expression),
              field('operator', choice('<', '<=', '>', '>=')),
              field('right', $.shift_expression)
          )
      )),

      // Shift: expression << expression, expression >> expression
      shift_expression: $ => prec.left(9, choice(
          $.additive_expression,
          seq(
              field('left', $.shift_expression),
              field('operator', choice('<<', '>>')),
              field('right', $.additive_expression)
          )
      )),

      // Additive: expression + expression, expression - expression
      additive_expression: $ => prec.left(10, choice(
          $.multiplicative_expression,
          seq(
              field('left', $.additive_expression),
              field('operator', choice('+', '-')),
              field('right', $.multiplicative_expression)
          )
      )),

      // Multiplicative: expression * expression, expression / expression, expression % expression
      multiplicative_expression: $ => prec.left(11, choice(
          $.unary_expression,
          seq(
              field('left', $.multiplicative_expression),
              field('operator', choice('*', '/', '%')),
              field('right', $.unary_expression)
          )
      )),

      // Unary: !expression, -expression, +expression
      unary_expression: $ => choice(
          $.postfix_expression,
          prec(12, seq(
              field('operator', choice('!', '-', '+')),
              field('operand', $.unary_expression)
          ))
      ),

      // Postfix: expression[index], expression(args)
      postfix_expression: $ => prec.left(13, choice(
          $.primary_expression,
          seq(
              field('object', $.postfix_expression),
              '[',
              field('index', $.expression),
              ']'
          ),
          $.function_call
      )),

      // Function call: identifier(args)
      function_call: $ => prec(14, seq(
          field('function', choice(
              $.builtin_function,
              $.identifier
          )),
          field('arguments', $.argument_list)
      )),

      // Primary expressions
      primary_expression: $ => choice(
          $.builtin_function,
          $.identifier,
          $.integer_literal,
          $.float_literal,
          $.string_literal,
          $.boolean_literal,
          $.null_literal,
          $.array_literal,
          $.hash_literal,
          $.function_literal,
          $.if_expression,
          $.parenthesized_expression,
      ),

      // Built-in functions
      builtin_function: $ => choice(
          'puts',
          'len',
          'first',
          'last',
          'rest',
          'push'
      ),

      // Identifier
      identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

      // Literals
      integer_literal: $ => /-?[0-9]+/,

      float_literal: $ => /-?[0-9]+\.[0-9]+/,

      string_literal: $ => seq(
          '"',
          repeat(choice(
              /[^"\\]/,
              seq('\\', /["\\/bfnrt]/)
          )),
          '"'
      ),

      boolean_literal: $ => choice('true', 'false'),

      null_literal: $ => 'null',

      // Array literal: [expression, expression, ...]
      array_literal: $ => seq(
          '[',
          optional(seq(
              $.expression,
              repeat(seq(',', $.expression)),
              optional(',')
          )),
          ']'
      ),

      // Hash literal: {key: value, key: value, ...}
      hash_literal: $ => seq(
          '{',
          optional(seq(
              $.hash_pair,
              repeat(seq(',', $.hash_pair)),
              optional(',')
          )),
          '}'
      ),

      hash_pair: $ => seq(
          field('key', $.expression),
          ':',
          field('value', $.expression)
      ),

      // Function statement: fn name(params) { body }
      function_statement: $ => seq(
          'fn',
          field('name', $.identifier),
          field('parameters', $.parameter_list),
          field('body', $.block_statement)
      ),

      // Function literal: fn(params) { body } (anonymous only)
      function_literal: $ => seq(
          'fn',
          field('parameters', $.parameter_list),
          field('body', $.block_statement)
      ),

      parameter_list: $ => seq(
          '(',
          optional(seq(
              $.identifier,
              repeat(seq(',', $.identifier)),
              optional(',')
          )),
          ')'
      ),

      argument_list: $ => seq(
          '(',
          optional(seq(
              $.expression,
              repeat(seq(',', $.expression)),
              optional(',')
          )),
          ')'
      ),

      // If expression: if (condition) { then_body } [else { else_body }]
      if_expression: $ => seq(
          'if',
          '(',
          field('condition', $.expression),
          ')',
          field('then_body', $.block_statement),
          optional(seq(
              'else',
              field('else_body', $.block_statement)
          ))
      ),

      // Parenthesized expression: (expression)
      parenthesized_expression: $ => seq(
          '(',
          $.expression,
          ')'
      ),
  }
});

import TokenType.*
import android.util.Log
import java.lang.RuntimeException
import java.lang.StringBuilder
import kotlin.system.exitProcess

var LOGGER = object: LangLogger {
    override fun log(text: String) {
        Log.d("GoAuto", "Interpreter: $text")
    }
}

var HAD_ERROR = false
var HAD_RUNTIME_ERROR = false

// TODO this is making things kinda hard to spot, returning null in theses case is not ideal but it's easier to spot when anything goes wrong - is there a better alternative?
val EMPTY_ANY = Any()
val VOID_ANY = Any()

class EmptyStmt<R> : Stmt<R>() {
    @Suppress("UNCHECKED_CAST")
    override fun accept(visitor: Visitor<R>): R {
        // Do nothing
        return EMPTY_ANY as R
    }
}

val RESERVED_KEYWORDS = hashMapOf(
    "and" to AND,
    "class" to CLASS,
    "else" to ELSE,
    "false" to FALSE,
    "for" to FOR,
    "fun" to FUN,
    "if" to IF,
    "nil" to NIL,
    "or" to OR,
    "return" to RETURN,
    "super" to SUPER,
    "this" to THIS,
    "true" to TRUE,
    "var" to VAR,
    "while" to WHILE,
    "print" to PRINT
)

enum class TokenType {
    // Single Characters
    SLASH,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, STAR,
    // One or two character tokens
    EQUAL,
    EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL, BANG, BANG_EQUAL,
    // Literals
    IDENTIFIER,
    STRING, NUMBER,
    // Keywords
    AND,
    CLASS, ELSE, FALSE, FUN, FOR, IF, NIL,
    OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
    EOF
}

interface LangLogger {
    fun log(text: String)
}

interface LangCallable {
    fun call(interpreter: Interpreter, arguments: List<Any>): Any

    /**
     * Returns the number of arguments that the callable is expected to have
     * */
    fun arity(): Int
}

class LangFunction(val declaration: Function<Any>) : LangCallable {
    override fun call(interpreter: Interpreter, arguments: List<Any>): Any {
        val environment = Environment(interpreter.globals)
        for (param in declaration.params) {
            environment.define(param.lexeme, arguments)
        }

        interpreter.executeBlock(declaration.body, environment)

        return EMPTY_ANY
    }

    override fun arity(): Int = declaration.params.size

    override fun toString(): String {
        return "<fn ${declaration.name.lexeme}>"
    }
}

data class Token(
    val type: TokenType,
    val lexeme: String,
    val literal: Any,
    val line: Int
)

class Scanner(val source: String) {
    private val tokens = arrayListOf<Token>()
    private var start = 0
    private var current = 0
    private var line = 1

    private val sourceChars = source.toCharArray()

    private fun isAtEnd(): Boolean {
        return current >= source.length
    }

    private fun addToken(type: TokenType, literal: Any = EMPTY_ANY) {
        val text = source.substring(start, current)
        tokens.add(Token(type, text, literal, line))
    }

    private fun advance(): Char {
        current += 1

        return sourceChars[current - 1]
    }

    private fun match(expected: Char): Boolean {
        if (isAtEnd()) return false
        if (sourceChars[current] != expected) return false
        current += 1
        return true
    }

    private fun peek(): Char {
        // null char if it's at the end
        if (isAtEnd()) return '\u0000'
        return sourceChars[current]
    }

    private fun string() {
        while (peek() != '"' && !isAtEnd()) {
            if (peek() == '\n') line++
            advance()
        }

        if (isAtEnd()) {
            error(line, "Unterminated string")
            return
        }

        // advance over the closing "
        advance()

        // remove starting " and ending "
        val value = source.substring(start + 1, current - 1)

        addToken(STRING, value)
    }

    private fun isDigit(c: Char): Boolean {
        return c in '0'..'9'
    }

    private fun peekNext(): Char {
        if (current + 1 >= source.length) return '\u0000'
        return sourceChars[current + 1]
    }

    private fun number() {
        while (isDigit(peek())) advance()

        if (peek() == '.' && isDigit(peekNext())) {
            // consume "."
            advance()
            // consume the rest of the number after .
            while (isDigit(peek())) advance()
        }

        val value = source.substring(start, current).toDouble()
        addToken(NUMBER, value)
    }

    private fun isAlpha(c: Char): Boolean {
        return c in 'a'..'z'
                || c in 'A'..'Z'
                || c == '_'
    }

    private fun isAlphaNumeric(c: Char): Boolean {
        return isAlpha(c) || isDigit(c)
    }

    private fun identifier() {
        while (isAlphaNumeric(peek())) advance()

        val text = source.substring(start, current)

        val type = RESERVED_KEYWORDS.get(text)

        if (type == null) {
            // Not a reserved keyword
            addToken(IDENTIFIER)
        } else {
            addToken(type)
        }
    }

    private fun scanToken() {
        when (val c = advance()) {
            '(' -> addToken(LEFT_PAREN)
            ')' -> addToken(RIGHT_PAREN)
            '{' -> addToken(LEFT_BRACE)
            '}' -> addToken(RIGHT_BRACE)
            ',' -> addToken(COMMA)
            '.' -> addToken(DOT)
            '-' -> addToken(MINUS)
            '+' -> addToken(PLUS)
            ';' -> addToken(SEMICOLON)
            '*' -> addToken(STAR)
            '!' -> if (match('=')) addToken(BANG_EQUAL) else addToken(BANG)
            '=' -> if (match('=')) addToken(EQUAL_EQUAL) else addToken(EQUAL)
            '<' -> if (match('=')) addToken(LESS_EQUAL) else addToken(LESS)
            '>' -> if (match('=')) addToken(GREATER_EQUAL) else addToken(GREATER)
            '/' -> {
                if (match('/')) {
                    // comment goes until the end of the line
                    while (peek() != '\n' && !isAtEnd()) {
                        advance()
                    }
                } else {
                    addToken(SLASH)
                }
            }
            ' ', '\r', '\t' -> {
                // ignore whitespace
            }
            '\n' -> line += 1
            '"' -> string()
            else -> {
                if (isDigit(c)) {
                    number()
                    return
                } else if (isAlpha(c)) {
                    identifier()
                    return
                }

                error(line, "advance received unhandled token $c")
            }
        }
    }

    fun scanTokens(): List<Token> {
        while (!isAtEnd()) {
            start = current
            scanToken()
        }

        tokens.add(Token(EOF, "", EMPTY_ANY, line))
        return tokens
    }
}

fun report(line: Int, where: String, message: String) {
    LOGGER.log("line: $line $where $message")
}

fun error(line: Int, message: String) {
    HAD_ERROR = true
    report(line, "", message)
}

/**
 * Notes on Grammar
 *
 * This uses a simplified notation for grammar.
 * For example check Backus-Naur form for a more formal version.
 *
 * Each rule:
 * name -> sequence_of_symbols
 *
 * Terminals are quoted strings, non-terminals lowercase words. Capitalized terminals are a single lexeme/token whose text can vary (numbers, strings, identifiers)
 *
 * A single rule can perform multiple productions.
 * A series of productions are separated by |
 *
 * () can be used to allow a single selection from a series of options.
 *
 * name -> ("option1" | "option2" | "option3") "another_option"
 *
 * A postfix * is used to mean that the symbol or group can be repeated zero or more times
 *
 * name -> "very" "very"*
 *
 * A postfix + is similar but it means it has to appear at least once
 *
 * name -> "very"+
 *
 * A postfix ? is used for an optional production
 *
 *
 * ---------------------------------------------
 *
 * Expressions in the language
 *
 * Literals - Numbers, strings, booleans, nil
 * Unary expressions - ! for a logical not, - to negate number
 * Binary expressions - infix arithmetic (+, -, *, /) and logic operators (==, !=, <, <=, >, >=)
 * Parentheses for grouping
 *
 *  Grammar for these:
 *
 * expression ->  literal
 *              | unary
 *              | binary
 *              | grouping ;
 *
 * literal  -> NUMBER | STRING | "true" | "false" | "nil" ;
 *
 * grouping -> "(" expression ")" ;
 * binary   -> expression operator expression ;
 * operator -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/" ;
 *
 */

/**
 * The Syntax tree classes are generated by a separate script to void the work of writing them by hand.
 */

/**
 * Syntax tree generator
 *
 * The classes below are generated from generate.kt
 */

// TODO is this a use case for sealed classes?


/**
 * Using the "visitor" pattern allows us to keep types untouched and have the all the behavior for a new operation in a single place. Since a new operation might interact across types, this avoids having to change each of types to support the new operation by having the implementation in a single place.
 * This is useful since these types will be used across boundaries, not only to to parse, but also in the interpreter.
 *
 * You can read a more concise explanation in the crafting interpreters book, but in summary, a interpreter pattern could be used, where each type is responsible to handle each of one of other types, by using the visitor pattern we can reverse the dependencies and have more flexibility by having a single place and not scattering the implementation across class methods.
 */

class AstPrinter : Visitor<String> {

    fun print(expr: Expr<String>): String {
        return expr.accept(this)
    }

    private fun parenthesize(name: String, vararg expressions: Expr<String>): String {
        val buf = StringBuilder()
        buf.append("(").append(name)
        expressions.forEach { buf.append(" ").append(it.accept(this)) }
        buf.append(")")
        return buf.toString()
    }

    override fun visitBinaryExpr(expr: Binary<String>): String {
        return parenthesize(expr.operator.lexeme, expr.left, expr.right)
    }

    override fun visitGroupingExpr(expr: Grouping<String>): String {
        return parenthesize("group", expr.expression)
    }

    override fun visitLiteralExpr(expr: Literal<String>): String {
        if (expr.value == EMPTY_ANY) return "nil"
        return expr.value.toString()
    }

    override fun visitUnaryExpr(expr: Unary<String>): String {
        return parenthesize(expr.operator.lexeme, expr.right)
    }

    override fun visitExpressionStmt(stmt: Expression<String>): String {
        return parenthesize("expression", stmt.expression)
    }

    override fun visitPrintStmt(stmt: Print<String>): String {
        return parenthesize("print", stmt.expression)
    }

    override fun visitVariableExpr(expr: Variable<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visitVarStmt(stmt: Var<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visitAssignExpr(expr: Assign<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visitBlockStmt(stmt: Block<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visitIfStmt(stmt: If<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visitLogicalExpr(expr: Logical<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visitWhileStmt(stmt: While<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visitCallExpr(expr: Call<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun visitFunctionStmt(stmt: Function<String>): String {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}


/**
 * Recursive descent (top-down) parser
 *
 *
 */

/**
 * Example of rules that below will be translated into the parser as class methods
 *
 * expression       -> equality ;
 * equality         -> comparison (("!=" | "==") comparison)* ;
 * comparison       -> addition ( (">" | ">=" | "<" | "<=" ) addition )* ;
 * addition         -> multiplication (("-" | "+") multiplication)* ;
 * multiplication   -> unary (( "/" | "*") unary)* ;
 * unary            -> ("!" | "-") unary | primary ;
 * primary          -> NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" ;
 *
 */

class ParseError : RuntimeException()

class Parser<R>(private val tokens: List<Token>) {
    private var current = 0

    fun parse(): List<Stmt<R>>? {
        /*return try {
            expression()
        } catch (e: ParseError) {
            null
        }*/

        val statements = arrayListOf<Stmt<R>>()
        while (!isAtEnd()) {
            statements.add(declaration())
        }

        return statements
    }

    private fun isAtEnd(): Boolean {
        return peek().type == EOF
    }

    private fun peek(): Token {
        return tokens[current]
    }

    private fun previous(): Token {
        return tokens[current - 1]
    }

    private fun advance(): Token {
        if (!isAtEnd()) current += 1
        return previous()
    }

    private fun check(type: TokenType): Boolean {
        if (isAtEnd()) return false

        return peek().type == type
    }

    private fun match(vararg types: TokenType): Boolean {
        for (type in types) {
            if (check(type)) {
                advance()
                return true
            }
        }
        return false
    }

    private fun declaration(): Stmt<R> {
        try {
            if (match(FUN)) return function("function")
            if (match(VAR)) return varDeclaration()

            return statement()
        } catch (error: ParseError) {
            synchronize()
            return EmptyStmt()
        }
    }

    private fun function(kind: String): Stmt<R> {
        val name = consume(IDENTIFIER, "Expected $kind name")

        consume(LEFT_PAREN, "Expect '(' after $kind name")

        val parameters = arrayListOf<Token>()

        if (!check(RIGHT_PAREN)) {
            do {
                if (parameters.size >= 255) {
                    error(peek(), "Cannot have more than 255 parameters")
                }

                parameters.add(consume(IDENTIFIER, "Expected parameter name"))
            } while (match(COMMA))
        }

        consume(RIGHT_PAREN, "Expect ')' after parameters")

        consume(LEFT_BRACE, "Expected '{' before $kind body")
        val body = block()

        return Function(name, parameters, body)
    }

    private fun statement(): Stmt<R> {
        if (match(FOR)) return forStatement()
        if (match(IF)) return ifStatement()
        if (match(PRINT)) return printStatement()
        if (match(WHILE)) return whileStatement()
        if (match(LEFT_BRACE)) return Block(block())
        return expressionStatement()
    }

    /**
     * the [FOR] statement is just a de-sugared while loop.
     * All elements, initializer, condition and increment are optional
     */
    private fun forStatement(): Stmt<R> {
        consume(LEFT_PAREN, "Expected '(' after while")
        val initializer: Stmt<R>?
        if (match(SEMICOLON)) {
            initializer = null
        } else if (match(VAR)) {
            initializer = varDeclaration()
        } else {
            initializer = expressionStatement()
        }

        var condition: Expr<R>? = null
        if (!check(SEMICOLON)) {
            condition = expression()
        }
        consume(SEMICOLON, "Expected ';' after loop condition")

        var increment: Expr<R>? = null
        if (!check(RIGHT_PAREN)) {
            increment = expression()
        }
        consume(RIGHT_PAREN, "Expecting ')' after for clauses")

        var body = statement()

        // Evaluate increment at the end of the block
        if (increment != null) {
            body = Block(arrayListOf(body, Expression(increment)))
        }

        if (condition == null) {
            condition = Literal(true)
        }

        body = While(condition, body)

        // Wrap the initialzer before the actual block of the for
        if (initializer != null) {
            body = Block(arrayListOf(initializer, body))
        }

        return body
    }

    private fun whileStatement(): Stmt<R> {
        consume(LEFT_PAREN, "Expected '(' after while")
        val condition = expression()
        consume(RIGHT_PAREN, "Expected ')' after while")
        val body = statement()

        return While(condition, body)
    }

    private fun ifStatement(): Stmt<R> {
        consume(LEFT_PAREN, "Expected '(' after 'if'")
        val condition = expression()
        consume(RIGHT_PAREN, "Expected ')' after if condition")

        val thenBranch = statement()
        var elseBranch: Stmt<R>? = null
        if (match(ELSE)) {
            elseBranch = statement()
        }

        return If(condition, thenBranch, elseBranch)
    }

    private fun block(): List<Stmt<R>> {
        val statements = arrayListOf<Stmt<R>>()

        while (!check(RIGHT_BRACE) && !isAtEnd()) {
            statements.add(declaration())
        }

        consume(RIGHT_BRACE, "Expecting '}' after block")
        return statements
    }

    private fun printStatement(): Stmt<R> {
        val value = expression()
        consume(SEMICOLON, "Expected ';' after value")
        return Print(value)
    }

    private fun expressionStatement(): Stmt<R> {
        val expr = expression()
        consume(SEMICOLON, "Expected ';' after expression")
        return Expression(expr)
    }

    private fun varDeclaration(): Stmt<R> {
        val name = consume(IDENTIFIER, "Expected variable name")

        var initializer: Expr<R>? = null
        if (match(EQUAL)) {
            initializer = expression()
        }
        consume(SEMICOLON, "Expected ';' after variable declaration")
        return Var(name, initializer)
    }

    private fun expression(): Expr<R> {
        return assignment()
    }

    private fun assignment(): Expr<R> {
        // Left hand side
        val expr = or()

        if (match(EQUAL)) {
            val equals = previous()

            // Handles chains in the l-value to find the expression and where it's assigned to. for example: newPoint(x + 2, 0).y = 3;
            // while also handling the evaluation of 'newPoint(x + 2, 0).y' by itself
            val value = assignment()

            if (expr is Variable) {
                val name = expr.name
                return Assign(name, value)
            }
            error(equals, "Invalid assignment target")
        }
        return expr
    }

    private fun or(): Expr<R> {
        var expr = and()

        while (match(OR)) {
            val operator = previous()
            val right = and()
            expr = Logical(expr, operator, right)
        }

        return expr
    }

    private fun and(): Expr<R> {
        var expr = equality()

        while (match(AND)) {
            val operator = previous()
            val right = equality()
            expr = Logical(expr, operator, right)
        }
        return expr
    }

    private fun equality(): Expr<R> {
        // equality -> comparison (("!=" | "==") comparison)* ;

        var expr = comparison()

        // check if an equality expression exists, for example: a == b == c == d
        while (match(BANG_EQUAL, EQUAL_EQUAL)) {
            val operator = previous()
            val right = comparison()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    private fun comparison(): Expr<R> {
        var expr = addition()

        while (match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            // previous since match already advanced the counter
            val operator = previous()
            val right = addition()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    private fun addition(): Expr<R> {
        var expr = multiplication()

        while (match(MINUS, PLUS)) {
            val operator = previous()
            val right = multiplication()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    // Note that this being deeper in the tree gives it precedence over addition for example
    private fun multiplication(): Expr<R> {
        var expr = unary()

        while (match(SLASH, STAR)) {
            val operator = previous()
            val right = unary()
            expr = Binary(expr, operator, right)
        }

        return expr
    }

    private fun unary(): Expr<R> {
        // this is a predictive parser, it looks ahead to decide which expression will be used in the tree
        if (match(BANG, MINUS)) {
            val operator = previous()
            val right = unary()
            return Unary(operator, right)
        }

        return call()
    }

    private fun call(): Expr<R> {

        var expr = primary()

        while (true) {
            if (match(LEFT_PAREN)) {
                expr = finishCall(expr)
            } else {
                break;
            }
        }
        return expr
    }

    private fun finishCall(callee: Expr<R>): Expr<R> {
        val arguments = arrayListOf<Expr<R>>()

        if (!check(RIGHT_PAREN)) {
            do {
                if (arguments.size >= 255) {
                    error(peek(), "Cannot have nore than 255 arguments")
                }
                arguments.add(expression())
            } while (match(COMMA))
        }

        val paren = consume(RIGHT_PAREN, "Expected ') after arguments")

        return Call(callee, paren, arguments)
    }

    private fun primary(): Expr<R> {
        when {
            match(FALSE) -> return Literal("false")
            match(TRUE) -> return Literal("true")
            match(NIL) -> return Literal(EMPTY_ANY)
            match(NUMBER, STRING) -> {
                return Literal(previous().literal)
            }
            match(IDENTIFIER) -> {
                return Variable(previous())
            }
            match(LEFT_PAREN) -> {
                val expr = expression()
                consume(RIGHT_PAREN, "Expected ')' after expression")
                return Grouping(expr)
            }
        }

        throwError(peek(), "Unhandled primary expression")
        // Unreachable
        return Literal(EMPTY_ANY)
    }

    private fun consume(type: TokenType, message: String): Token {
        if (check(type)) return advance()

        throwError(peek(), message)
        // Unreachable
        return EMPTY_ANY as Token
    }

    private fun throwError(token: Token, message: String) {
        error(token, message)
        throw ParseError()
    }

    private fun error(token: Token, message: String) {
        HAD_ERROR = true

        if (token.type == EOF) {
            report(token.line, " at end", message)
        } else {
            report(token.line, " at '${token.lexeme}'", message)
        }
    }

    /**
     * After we find a parser error, we will attempt to continue parsing (so we can catch further errors if possible) by finding a synchronization point, in this case a semicolon is a good indicative when an expression ends - this won't work for semicolons in the middle of a for() expression but this is already in best effort.
     *
     * This will take advantage of stack unwinding after [ParseError] is caught which might not be ideal for portability to another languages but it will have to serve for now.
     */
    private fun synchronize() {
        advance()

        while (!isAtEnd()) {
            if (previous().type == SEMICOLON) return

            when (peek().type) {
                CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN -> return
                else -> {
                    advance()
                }
            }
        }
    }
}

// TODO unreachable returns should probably report an error, currently we're silently returning an invalid value

class RuntimeError(val token: Token, override val message: String) : RuntimeException(message)

class Interpreter : Visitor<Any> {
    val globals = Environment()
    // Might be switched around temporarily while blocks
    // are being evaluated
    private var environment = globals

    init {
        // Default methods implementation
        // these occupy the same namespace as variables

        globals.define("clock", object : LangCallable {
            override fun call(interpreter: Interpreter, arguments: List<Any>): Any {
                return System.currentTimeMillis() / 1000.0
            }

            override fun arity(): Int = 0
        })

        globals.define("str", object : LangCallable {
            override fun call(interpreter: Interpreter, arguments: List<Any>): Any {
                val arg = arguments[0]
                if (arg is Double) {
                    var text = arg.toString()
                    if (text.endsWith(".0")) {
                        // Since we use use a double to represent integers, remove the .0 to get an integer back
                        text = text.substring(0, text.length - 2)
                    }
                    return text
                }

                return arg.toString()
            }

            override fun arity(): Int = 1
        })
    }

    fun interpret(statements: List<Stmt<Any>>) {
        try {
            for (statement in statements) {
                execute(statement)
            }
        } catch (error: RuntimeError) {
            runtimeError(error)
        }
    }

    private fun execute(stmt: Stmt<Any>) {
        stmt.accept(this)
    }

    override fun visitBinaryExpr(expr: Binary<Any>): Any {
        val left = evaluate(expr.left)
        val right = evaluate(expr.right)

        return when (expr.operator.type) {
            MINUS -> {
                checkNumberOperands(expr.operator, left, right)
                (left as Double) - (right as Double)
            }
            SLASH -> {
                checkNumberOperands(expr.operator, left, right)
                (left as Double) / (right as Double)
            }
            STAR -> {
                checkNumberOperands(expr.operator, left, right)
                (left as Double) * (right as Double)
            }
            PLUS -> {
                return if (left is Double && right is Double) {
                    left + right
                } else if (left is String && right is String) {
                    left + right
                } else {
                    throw RuntimeError(expr.operator, "Operands must be two numbers or two strings")
                    // Unreachable
                    EMPTY_ANY
                }
            }
            GREATER -> {
                checkNumberOperands(expr.operator, left, right)
                (left as Double) > (right as Double)
            }
            GREATER_EQUAL -> {
                checkNumberOperands(expr.operator, left, right)
                (left as Double) >= (right as Double)
            }
            LESS -> {
                checkNumberOperands(expr.operator, left, right)
                (left as Double) < (right as Double)
            }
            LESS_EQUAL -> {
                checkNumberOperands(expr.operator, left, right)
                (left as Double) <= (right as Double)
            }
            BANG_EQUAL -> !isEqual(left, right)
            EQUAL_EQUAL -> isEqual(left, right)
            else -> {
                // Unreachable
                EMPTY_ANY
            }
        }
    }

    override fun visitGroupingExpr(expr: Grouping<Any>): Any {
        return evaluate(expr.expression)
    }

    override fun visitLiteralExpr(expr: Literal<Any>): Any {
        return expr.value
    }

    override fun visitUnaryExpr(expr: Unary<Any>): Any {
        val right = evaluate(expr.right)

        return when (expr.operator.type) {
            MINUS -> {
                checkNumberOperand(expr.operator, right)
                -(right as Double)
            }
            BANG -> !isTruthy(right)
            else -> {
                // Unreachable
                EMPTY_ANY
            }
        }
    }

    private fun evaluate(expr: Expr<Any>): Any {
        return expr.accept(this)
    }

    private fun isTruthy(obj: Any): Boolean {
        if (obj == EMPTY_ANY) return false
        if (obj is Boolean) return obj
        return true
    }

    private fun isEqual(a: Any, b: Any): Boolean {
        // nil is only equal to nil
        return if (a == EMPTY_ANY && b == EMPTY_ANY) {
            true
        } else if (a == EMPTY_ANY) {
            false
        } else {
            a == b
        }
    }

    private fun checkNumberOperand(operator: Token, operand: Any) {
        if (operand is Double) return
        throw RuntimeError(operator, "Operand must be a number")
    }

    private fun checkNumberOperands(operator: Token, left: Any, right: Any) {
        if (left is Double && right is Double) return

        throw RuntimeError(operator, "Operands must be numbers")
    }

    private fun stringify(obj: Any): String {
        if (obj == EMPTY_ANY) return "nil"
        if (obj is Double) {
            var text = obj.toString()
            if (text.endsWith(".0")) {
                // Since we use use a double to represent integers, remove the .0 to get an integer back
                text = text.substring(0, text.length - 2)
            }
            return text
        }

        return obj.toString()
    }

    private fun runtimeError(error: RuntimeError) {
        LOGGER.log("${error.message} at line ${error.token.line}")
        HAD_RUNTIME_ERROR = true
    }

    override fun visitExpressionStmt(stmt: Expression<Any>): Any {
        evaluate(stmt.expression)

        // In java instead of Any we would have Object used generally and we would have 'Void' as the return type (in which we would return null at this point)
        return VOID_ANY
    }

    override fun visitPrintStmt(stmt: Print<Any>): Any {
        val value = evaluate(stmt.expression)
        LOGGER.log(stringify(value))
        return VOID_ANY
    }

    override fun visitVariableExpr(expr: Variable<Any>): Any {
        return environment.get(expr.name)
    }

    override fun visitVarStmt(stmt: Var<Any>): Any {
        if (stmt.initializer != null) {
            val value = evaluate(stmt.initializer)
            environment.define(stmt.name.lexeme, value)
        } else {
            // not explicitly initialized
            environment.define(stmt.name.lexeme, VOID_ANY)
        }

        return VOID_ANY
    }

    override fun visitAssignExpr(expr: Assign<Any>): Any {
        val value = evaluate(expr.value)
        environment.assign(expr.name, value)
        return value
    }

    override fun visitBlockStmt(stmt: Block<Any>): Any {
        executeBlock(stmt.statement, Environment(environment))
        return VOID_ANY
    }

    fun executeBlock(statements: List<Stmt<Any>>, environment: Environment) {
        val previous = environment

        try {
            this.environment = environment

            for (statement in statements) {
                execute(statement)
            }

        } finally {
            this.environment = previous
        }
    }

    override fun visitIfStmt(stmt: If<Any>): Any {
        if (isTruthy(evaluate(stmt.condition))) {
            execute(stmt.thenBranch)
        } else if (stmt.elseBranch != null) {
            execute(stmt.elseBranch)
        }

        return VOID_ANY
    }

    override fun visitLogicalExpr(expr: Logical<Any>): Any {
        val left = evaluate(expr.left)

        if (expr.operator.type == OR) {
            // Short circuit
            if (isTruthy(left)) return left
        } else {
            if (!isTruthy(left)) return left
        }

        return evaluate(expr.right)
    }

    override fun visitWhileStmt(stmt: While<Any>): Any {
        while (isTruthy(evaluate(stmt.condition))) {
            execute(stmt.body)
        }
        return VOID_ANY
    }

    override fun visitCallExpr(expr: Call<Any>): Any {
        val function = evaluate(expr.callee)

        val arguments = arrayListOf<Any>()

        for (argument in expr.arguments) {
            arguments.add(evaluate(argument))
        }

        if (function !is LangCallable) {
            throw RuntimeError(expr.paren, "Can't call this")
        }

        // Verify that the number of arguments match before calling it (during runtime)
        if (arguments.size != function.arity()) {
            throw RuntimeError(expr.paren, "Expected ${function.arity()} but got ${arguments.size} arguments")
        }

        return function.call(this, arguments)
    }

    override fun visitFunctionStmt(stmt: Function<Any>): Any {
        val function = LangFunction(stmt)
        environment.define(stmt.name.lexeme, function)
        return VOID_ANY
    }
}

fun runProgram(source: String, functions: List<Pair<String, LangCallable>> = arrayListOf()) {
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()
    val parser = Parser<Any>(tokens)
    val statements = parser.parse()

    if (HAD_ERROR) {
        LOGGER.log("Some kind of syntax error happened")
        return
    }

    if (statements == null) {
        LOGGER.log("No parsed expression")
    } else {
        val interpreter = Interpreter()
        for (function in functions) {
            interpreter.globals.define(function.first, function.second)
        }
        interpreter.interpret(statements)
    }
}

class Environment(val enclosing: Environment? = null) {
    private val values = hashMapOf<String, Any>()

    fun define(name: String, value: Any) {
        values[name] = value
    }

    fun get(name: Token): Any {
        if (values.containsKey(name.lexeme)) {
            return values[name.lexeme]!!
        }

        if (enclosing != null) return enclosing.get(name)

        throw RuntimeError(name, "Undefined variable '${name.lexeme}'")
    }

    fun assign(name: Token, value: Any) {
        if (values.containsKey(name.lexeme)) {
            values[name.lexeme] = value
            return
        }

        if (enclosing != null) {
            enclosing.assign(name, value)
            return
        }

        throw RuntimeError(name, "Undefined variable '${name.lexeme}'")
    }
}

data class Example<T>(val source: String, val expectedResult: T)

fun main() {
    LOGGER.log("HI")

    val cases = arrayOf(
        Example(
            "print 4 * 1 + 2 * 3;", 10
        ),
        Example(
            "print \"hello \" + \"world\";", "hello world"
        ),
        Example(
            "print false == false;", "true"
        ),
        Example(
            "print 10 == 10.0;", "true"
        ),
        Example(
            "print 10 == 10;", "true"
        ),
        Example(
            "print 10.1 == 10;", "false"
        ),
        Example(
            """
                        var a = 1;
                        var b = 2;
                        print a + b;
                    """.trimIndent(), "3"
        ),
        Example(
            """
                        var b = 2 * 2;
                        print b;
                    """.trimIndent(), "4"
        ),
        Example(
            """
                        var b = 2 * 2;
                        var c = b * 2;
                        print c;
                    """.trimIndent(), "8"
        ),
        Example(
            """
                        var a = "global a";
                        var b = "global b";
                        var c = "global c";
                        {
                            var a = "outer  a";
                            var b = "outer  b";
                                {
                                    var a = "inner  a";
                                    print a;
                                    print b;
                                    print c;
                                }
                            print a;
                            print b;
                            print c;
                        }
                        print a;
                        print b;
                        print c;
                    """.trimIndent(), ""
        ),
        Example(
            """
                        var b = 1;
                        var c = 1;
                        if (b == c) {
                            print "Hello";
                        }
                        var d = 4;
                        if (d == b) {
                            print "Sailor";
                        } else {
                            print "World";
                        }
                    """.trimIndent(), "Hello\nWorld"
        ),
        Example(
            """
                        print nil or "Just";
                        var a = "Sayin";
                        
                        if (a == "Sayin" or non_existent_thing_that_should_not_be_evaluated == false) {
                            print a;
                        }
                    """.trimIndent(), "Just\nSayin"
        ),
        Example(
            """
                        var a = 0;
                        var b = 1;
                        while (a < 1000) {
                            print a;
                            var t = a;
                            a = b;
                            b = t + b;
                        }
                    """.trimIndent(), ""
        ),
        Example(
            """
                        for (var i = 0; i <= 10; i = i +1) {
                            print i;
                        }
                    """.trimIndent(), ""
        ),
        Example(
            """
                        var start = clock();
                        print "Hello";
                        for(var i = 0; i < 500; i = i + 1) {}
                        var end = clock();
                        var diff = end - start;
                        print "start: " + str(start) + " end: " + str(end) + " diff: " + str(diff) + " seconds";
                    """.trimIndent(), ""
        ),
        Example(
            """
                        fun add(a) {
                            print a;
                        }
                        
                        print add;
                        print add(444);
                    """.trimIndent(), ""
        )
    )

    val usedCases =
        //cases
        arrayListOf(cases.last())

    for (case in usedCases) {

        LOGGER.log("--- case ---\n${case.source} | expected '${case.expectedResult}' | Got:")
        runProgram(case.source)

        if (HAD_ERROR) exitProcess(65)
        if (HAD_RUNTIME_ERROR) exitProcess(70)
    }
}
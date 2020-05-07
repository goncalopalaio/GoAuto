// region GENERATED CODE

interface Visitor<R> {
    fun visitAssignExpr(expr: Assign<R>): R
    fun visitBinaryExpr(expr: Binary<R>): R
    fun visitCallExpr(expr: Call<R>): R
    fun visitGroupingExpr(expr: Grouping<R>): R
    fun visitLiteralExpr(expr: Literal<R>): R
    fun visitLogicalExpr(expr: Logical<R>): R
    fun visitUnaryExpr(expr: Unary<R>): R
    fun visitVariableExpr(expr: Variable<R>): R
    fun visitBlockStmt(stmt: Block<R>): R
    fun visitExpressionStmt(stmt: Expression<R>): R
    fun visitFunctionStmt(stmt: Function<R>): R
    fun visitIfStmt(stmt: If<R>): R
    fun visitPrintStmt(stmt: Print<R>): R
    fun visitVarStmt(stmt: Var<R>): R
    fun visitWhileStmt(stmt: While<R>): R
}

abstract class Expr<R> {
    abstract fun accept(visitor: Visitor<R>): R
}

class Assign<R>(val name: Token, val value: Expr<R>) : Expr<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitAssignExpr(this)
    }
}

class Binary<R>(val left: Expr<R>, val operator: Token, val right: Expr<R>) : Expr<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitBinaryExpr(this)
    }
}

class Call<R>(val callee: Expr<R>, val paren: Token, val arguments: List<Expr<R>>) : Expr<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitCallExpr(this)
    }
}

class Grouping<R>(val expression: Expr<R>) : Expr<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitGroupingExpr(this)
    }
}

class Literal<R>(val value: Any) : Expr<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitLiteralExpr(this)
    }
}

class Logical<R>(val left: Expr<R>, val operator: Token, val right: Expr<R>) : Expr<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitLogicalExpr(this)
    }
}

class Unary<R>(val operator: Token, val right: Expr<R>) : Expr<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitUnaryExpr(this)
    }
}

class Variable<R>(val name: Token) : Expr<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitVariableExpr(this)
    }
}

abstract class Stmt<R> {
    abstract fun accept(visitor: Visitor<R>): R
}

class Block<R>(val statement: List<Stmt<R>>) : Stmt<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitBlockStmt(this)
    }
}

class Expression<R>(val expression: Expr<R>) : Stmt<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitExpressionStmt(this)
    }
}

class Function<R>(val name: Token, val params: List<Token>, val body: List<Stmt<R>>) : Stmt<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitFunctionStmt(this)
    }
}

class If<R>(val condition: Expr<R>, val thenBranch: Stmt<R>, val elseBranch: Stmt<R>?) : Stmt<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitIfStmt(this)
    }
}

class Print<R>(val expression: Expr<R>) : Stmt<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitPrintStmt(this)
    }
}

class Var<R>(val name: Token, val initializer: Expr<R>?) : Stmt<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitVarStmt(this)
    }
}

class While<R>(val condition: Expr<R>, val body: Stmt<R>) : Stmt<R>() {
    override fun accept(visitor: Visitor<R>): R {
        return visitor.visitWhileStmt(this)
    }
}

// endregion GENERATED CODE



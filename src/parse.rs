use crate::ast::*;
use pest::iterators::Pair;
use crate::Rule;

impl ProgramNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        let mut decls: Vec<Box<dyn DeclNode>> = Vec::new();
        for pair in inner_pairs {
            if let Rule::decl = pair.as_rule() {
                decls.push(generate_decl_node(pair));
            }
        }
        ProgramNode(decls)
    }
}

impl ASTNode for ProgramNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

impl VarDeclNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        VarDeclNode {
            typ: Type::from(inner_pairs().next().unwrap()),
            id: IDNode::from(inner_pairs().next().unwrap()),
        }
    }
}

impl ASTNode for VarDeclNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

impl FnDeclNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        let typ = Type::from(inner_pairs().next().unwrap());
        let id = IDNode::from(inner_pairs().next().unwrap());
        let mut formals: Vec<FormalDeclNode> = Vec::new();
        let mut stmts: Vec<Box<dyn StmtNode>>;
        let maybe_formals = inner_pairs.nth(1);
        let stmt_list_pair;
        if let Rule::formals = maybe_formals.as_rule() {
            for formal in maybe_formals.into_inner() {
                if let Rule::formalDecl = formal.as_rule() {
                    formals.push(FormalDeclNode::from(formal));
                }
            }
            stmt_list_pair = inner_pairs.nth(2).unwrap();
        } else {
            stmt_list_pair = inner_pairs.nth(1).unwrap();
        }
        stmts = generate_stmt_list(stmt_list_pair);
        FnDeclNode { typ, id, formals, stmts }
    }
}

impl ASTNode for FnDeclNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

impl Type {
    pub fn from(pair: Pair<Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        let fst = inner_pairs.next().unwrap();
        match fst.as_rule() {
            Rule::PTR => Type::Ptr(
                Box::new(Type::from(inner_pairs.next().unwrap()))
            ),
            Rule::primType => {
                match fst.into_inner().next().unwrap().as_rule() {
                    Rule::INT => Type::Int,
                    Rule::BOOL => Type::Bool,
                    Rule::STRING => Type::Str,
                    Rule::SHORT => Type::Short,
                    Rule::VOID => Type::Void,
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }
}

impl ASTNode for Type {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

impl FormalDeclNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        FormalDeclNode {
            typ: Type::from(inner_pairs.next().unwrap()),
            id: IDNode::from(inner_pairs.next().unwrap()),
        }
    }
}

impl ASTNode for FormalDeclNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}


impl ASTNode for BinaryExpNode {
    pub fn to_string(&self) -> String {
        let op_str = match self.op {
            BinaryOperator::And => "and",
            BinaryOperator::Divide => "/",
            BinaryOperator::Equals => "==",
            BinaryOperator::GreaterEq => ">=",
            BinaryOperator::Greater => ">",
            BinaryOperator::LessEq => "<=",
            BinaryOperator::Less => "<",
            BinaryOperator::Minus => "-",
            BinaryOperator::NotEquals => "!=",
            BinaryOperator::Or => "or",
            BinaryOperator::Plus => "+",
            BinaryOperator::Times => "*",
        }.to_string();
        let lhs_str = lhs.to_string();
        let rhs_str = rhs.to_string();
        format!("{} {} {}", lhs_str, op_str, rhs_str)
    }
}

impl IDNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        IDNode(pair.as_str().to_string())
    }
}

impl ASTNode for IDNode {
    pub fn to_string(&self) -> String {
        self.0
    }
}

impl IntLitNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        IntLitNode(pair.as_str().parse().unwrap())
    }
}

impl ASTNode for IntLitNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

impl ShortLitNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let s = pair.as_str();
        let len = s.len();
        ShortLitNode(s[..len-1].parse().unwrap())
    }
}

impl ASTNode for ShortLitNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

impl StrLitNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        StrLitNode(pair.as_str().to_string())
    }
}

impl ASTNode for StrLitNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

impl AssignExpNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        let lval = generate_lval_node(inner_pairs.next().unwrap());
        let exp = generate_exp_node(inner_pairs.nth(1).unwrap());
        AssignExpNode { lval, exp }
    }
}

impl ASTNode for AssignExpNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

impl CallExpNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        let id = inner_pairs.next();
        let mut args: Vec<Box<dyn ExpNode>> = Vec::new();
        let maybe_actuals_pair = inner_pairs.nth(1).unwrap();
        if let Rule::actualsList = maybe_actuals_pair.as_rule() {
            let mut pairs = maybe_actuals_pair.into_inner()
            while let Some(pair) = pairs.next(){
                args.push(generate_exp_node(pair));
                pairs.next();
            }
        }
        CallExpNode { id, args }
    }
}

impl ASTNode for CallExpNode {
    pub fn to_string(&self) -> String {
        todo!()
    }
}

pub fn generate_exp_node(exp_pair: Pair<Rule>) -> Box<dyn ExpNode> {
    match exp_pair.as_rule() {
        Rule::exp => generate_exp_node(exp_pair.into_inner()),
        Rule::assignExp => Assigngenerate_exp_node(pair),
        Rule::boolExp
        | Rule::logicTerm
        | Rule::compareExp
        | Rule::sum
        | Rule::term => {
            // There are two cases here. The first is that we are
            // looking at a pair which has a chain of one or more 
            // operators. The second is that we're just looking at 
            // a singular factor from above.
            let mut inner_pairs = exp_pair.into_inner();
            let mut lhs: = generate_exp_node(inner_pairs.next().unwrap())
            while let Some(pair) = inner_pairs.next() {
                let op = match pair.as_rule() {
                    Rule::OR => BinaryOperator::Or,
                    Rule::AND => BinaryOperator::And,
                    Rule::EQUALS => BinaryOperator::Equals,
                    Rule::NOTEQUALS => BinaryOperator::NotEquals,
                    Rule::GREATER => BinaryOperator::Greater,
                    Rule::GREATEREQ => BinaryOperator::GreaterEq,
                    Rule::LESS => BinaryOperator::Less,
                    Rule::LESSEQ => BinaryOperator::LessEq,
                    Rule::PLUS => BinaryOperator::Plus,
                    Rule::MINUS => BinaryOperator::Minus,
                    Rule::TIMES => BinaryOperator::Times,
                    Rule::DIVIDE => BinaryOperator::Divide,
                    _ => unreachable!(),
                };
                let rhs = generate_exp_node(inner_pairs.next().unwrap());
                lhs = Box::new(BinaryExpNode { op, lhs, rhs });
            }
            lhs
        }
        Rule::factor => {
            let inner_pairs = exp_pair.into_inner();
            let fst = inner_pairs.next().unwrap();
            match fst.as_rule() {
                // Trivial cases first, where it's just one rule
                Rule::assignExp => AssignExpNode::from(fst),
                Rule::callExp => CallExpNode::from(fst),
                Rule::LValNode => generate_lval_node(fst),
                // Now, choices which are made up of a composition of
                // rules
                Rule::NOT => Box::new(UnaryExpNode { 
                    op: UnaryOp::Not,
                    exp: generate_exp_node(inner_pairs.next().unwrap()),
                }),
                Rule::MINUS => Box::new(UnaryExpNode {
                    op: UnaryOp::Neg,
                    exp: generate_exp_node(inner_pairs.next().unwrap()),
                }),
                Rule::LPAREN => generate_exp_node(
                    inner_pairs.next().unwrap()
                ),
                Rule::AMP => Box::new(UnaryExpNode {
                    op: UnaryOp::Ref,
                    id: IDNode::from(inner_pairs.next().unwrap()),
                }),
                // Now literals
                Rule::INTLITERAL => Box::new(IntLitNode::from(fst)),
                Rule::SHORTLITERAL => Box::new(ShortLitNode::from(fst)),
                Rule::STRLITERAL => Box::new(StrLitNode::from(fst)),
                Rule::TRUE => Box::new(TrueNode),
                Rule::FALSE => Box::new(FalseNode),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

pub fn generate_lval_node(pair: Pair<Rule>) -> Box<dyn LValNode> {
    let mut inner_pairs = pair.into_inner();
    let fst = inner_pairs.next();
    Box::new(match fst.as_rule() {
        Rule::AT => DerefNode(IDNode::from(inner_pairs.next().unwrap())),
        Rule::id => IDNode::from(fst),
        _ => unreachable!(),
    })
}

pub fn generate_decl_node(pair: Pair<Rule>) -> Box<dyn DeclNode> {
    let inner_pair = pair.into_inner().next().unwrap();
    match inner_pair.as_rule() {
        Rule::varDecl => VarDeclNode::from(inner_pair),
        Rule::fnDecl => FnDeclNode::from(inner_pair),
        _ => unreachable!(),
    }
}

pub fn generate_stmt_list(pair: Pair<Rule>) -> Vec<Box<dyn StmtNode>> {
    let mut ret: Vec<Box<dyn StmtNode>> = Vec::new();
    for p in pair.into_inner() {
        vec.push(generate_stmt_node(p));
    }
    vec
}

pub fn generate_stmt_node(pair: Pair<Rule>) -> Box<dyn StmtNode> {
    let inner_pairs = pair.into_inner();
    let fst = inner_pairs.next().unwrap();
    match fst.as_rule() {
        Rule::varDecl => Box::new(VarDeclNode::from(fst)),
        Rule::assignExp => Box::new(AssignExpNode::from(fst)),
        Rule::lval => match inner_pairs.next().unwrap().as_rule() {
            Rule::DEC => Box::new(PostDecNode(fst)),
            Rule::INC => Box::new(PostIncNode(fst)),
            _ => unreachable!(),
        },
        Rule::READ => generate_lval_node(
            inner_pairs.next().unwrap()
        ),
        Rule::WRITE => generate_exp_node(
            inner_pairs.next().unwrap()
        ),
        Rule::IF => {
            let exp = generate_exp_node(inner_pairs().nth(1));
            let true_stmts = generate_stmt_list(
                inner_pairs.nth(2).unwrap()
            );
            let maybe_else = inner_pairs().nth(1);
            if let Rule::ELSE = maybe_else.unwrap().as_rule() {
                let else_stmts = generate_stmt_list(
                    inner_pairs.nth(1).unwrap()
                );
                IfElseStmtNode { exp, true_stmts, else_stmts }
            } else {
                IfStmtNode { exp, stmts: true_stmts }
            }
        }
        Rule::WHILE => {
            let exp = generate_exp_node(inner_pairs.nth(1).unwrap());
            let stmts = generate_stmt_list(inner_pairs.nth(2).unwrap());
            WhileStmtNode { exp, stmts }
        }
        Rule::RETURN => {
            let maybe_exp = inner_pairs.next().unwrap();
            match maybe_exp.as_rule() {
                Rule::exp => ReturnStmtNode(
                    Some(generate_exp_node(maybe_exp))
                ),
                _ => None,
            }
        }
        Rule::callExp => CallExpNode::from(fst),
        _ => unreachable!(),
    }
}

use pest::prec_climber::{PrecClimber, Assoc, Operator};
use lazy_static::lazy_static;

#[derive(Parser)]
#[grammar = "cmm.pest"]
pub struct CMMParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(OR, Left),
            Operator::new(AND, Left),
            Operator::new(EQUALS, Left)
            | Operator::new(NOTEQUALS, Left)
            | Operator::new(GREATER, Left)
            | Operator::new(GREATEREQ, Left)
            | Operator::new(LESS, Left)
            | Operator::new(LESSEQ, Left),
            Operator::new(MINUS, Left) | Operator::new(PLUS, Left),
            Operator::new(TIMES, Left) | Operator::new(DIVIDE, Left)
        ])
    };
}


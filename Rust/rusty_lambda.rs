use std::fmt::{Display, Error, Formatter};
use Term::*;

// Combination of FnOnce(Term) -> Term and Clone
trait TermLam {
    // The FnOnce part, declared like an Fn, because we need object safety
    fn app(&self, t: Term) -> Term;
    // The Clone part, but we have to return sized objects
    // (not Self either because of object safety), so it is in a box
    fn clone_box(&self) -> Box<dyn TermLam>;
}

// Blanket implementation for appropriate types
impl<F> TermLam for F
where
    F: 'static/*' highlighting fix */ + Clone + FnOnce(Term) -> Term,
{
    // Note: when you have a Clone + FnOnce, you effectively have an Fn
    fn app(&self, t: Term) -> Term {
        (self.clone())(t)
    }

    fn clone_box(&self) -> Box<dyn TermLam> {
        Box::new(self.clone())
    }
}

// We can now clone the box
impl Clone for Box<dyn TermLam> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

#[derive(Clone)]
enum Term {
    Hol(Box<Term>),
    Var(usize),
    Lam(Box<dyn TermLam>),
    App(Box<Term>, Box<Term>),
}

impl Term {
    fn app(t1: Term, t2: Term) -> Self {
        App(Box::new(t1), Box::new(t2))
    }

    fn lam<F>(f: F) -> Self
    where
        F: 'static/*' highlighting fix */ + Clone + FnOnce(Term) -> Term,
    {
        Lam(Box::new(f))
    }

    fn hol(t: Term) -> Self {
        Hol(Box::new(t))
    }

    // `reduce` is now a by-reference method
    fn reduce(&self) -> Term {
        match self {
            Hol(_) => self.clone(),
            Var(_) => self.clone(),
            Lam(bod) => {
                let bod = bod.clone();
                Term::lam(move |v| bod.app(v).reduce())
            },
            // We reuse the reduced object when possible,
            // to avoid unnecessary clone.
            App(fun, arg) => match fun.reduce() {
                other @ Hol(_) => Term::app(other, arg.reduce()),
                other @ Var(_) => Term::app(other, arg.reduce()),
                Lam(fbod) => fbod.app(arg.reduce()),
                other @ App(_, _) => Term::app(other, arg.reduce()),
            },
        }
    }
}
//The standard way of `pretty` is `Display`
impl Display for Term {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        // As the API is different from `pretty`, the way we do recursion is
        // a bit different as well
        struct LvlTerm<'a>(usize, &'a Term);
        impl<'a> Display for LvlTerm<'a> {
            fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
                match self {
                    LvlTerm(lvl, Hol(hol)) => write!(fmt, "{}", LvlTerm(*lvl, hol)),
                    LvlTerm(_, Var(idx)) => write!(fmt, "x{}", idx),
                    LvlTerm(lvl, Lam(bod)) => write!(
                        fmt,
                        "λx{}. {}",
                        *lvl,
                        LvlTerm(*lvl + 1, &bod.app(Term::hol(Var(*lvl))))
                    ),
                    LvlTerm(lvl, App(fun, arg)) => {
                        write!(fmt, "({} {})", LvlTerm(*lvl, fun), LvlTerm(*lvl, arg))
                    }
                }
            }
        }
        write!(fmt, "{}", LvlTerm(0, self))
    }
}

fn main() {
    // In general, if you need to use a value n+1 times, you need to
    // call clone it n times. You don't have to clone it in the last use.
    // (λx. x x) (λs. λz. s (s (s z)))
    let term1 = Term::app(
        Term::lam(|x| Term::app(x.clone(), x)),
        Term::lam(|s| {
            Term::lam(move |z| Term::app(s.clone(), Term::app(s.clone(), Term::app(s, z))))
        }),
    );

    // No clone is required if all values are used exactly once.
    // λb. λt. λf. b t f
    let term2 =
        Term::lam(|b| Term::lam(move |t| Term::lam(move |f| Term::app(Term::app(b, t), f))));

    println!("{}", term1.reduce()); // λx0. λx1. (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 x1)))))))))))))))))))))))))))
    println!("{}", term2.reduce()); // λx0. λx1. λx2. ((x0 x1) x2)
}

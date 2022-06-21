use std::rc::Rc;
use Term::*;

#[derive(Clone)]
enum Term {
    Hol(Box<Term>),
    Var(usize),
    Lam(Rc<dyn Fn(Term) -> Term>),
    App(Box<Term>, Box<Term>),
}

impl Term {
    fn app(t1: Term, t2: Term) -> Self {
        App(Box::new(t1), Box::new(t2))
    }

    fn lam<F: Fn(Term) -> Term + 'static>(f: F) -> Self {
        Lam(Rc::new(f))
    }

    fn hol(t: Term) -> Self {
        Hol(Box::new(t))
    }
}

fn pretty(term: Term) -> String {
    fn go(lvl: usize, term: Term) -> String {
        match term {
            Hol(hol) => go(lvl, *hol),
            Var(idx) => format!("x{}", idx),
            Lam(bod) => format!("λx{}. {}", lvl, go(lvl + 1, bod(Term::hol(Var(lvl))))),
            App(fun, arg) => format!("({} {})", go(lvl, *fun), go(lvl, *arg)),
        }
    }

    go(0, term)
}

fn reduce(term: Term) -> Term {
    match term {
        Hol(hol) => *hol,
        Var(idx) => Var(idx),
        Lam(bod) => Term::lam(move |v| reduce(bod(v))),
        App(fun, arg) => match reduce(*fun) {
            Hol(fhol) => Term::app(Hol(fhol), reduce(*arg)),
            Var(fidx) => Term::app(Var(fidx), reduce(*arg)),
            Lam(fbod) => fbod(reduce(*arg)),
            App(ffun, farg) => Term::app(Term::app(*ffun, *farg), reduce(*arg)),
        },
    }
}

fn main() {
    // (λx. x x) (λs. λz. s (s (s z)))
    let term1 = Term::app(
        Term::lam(|x| Term::app(x.clone(), x.clone())), 
        Term::lam(|s| Term::lam(move |z| 
            Term::app(
                s.clone(),
                Term::app(
                    s.clone(),
                    Term::app(
                        s.clone(),
                        z.clone()
    ))))));
    
    // λb. λt. λf. b t f
    let term2 = Term::lam(|b| Term::lam(move |t| 
        Term::lam({
            let b = b.clone(); // necessary to satisfy the borrow checker
            move |f| Term::app(Term::app(b.clone(), t.clone()), f)
        })
    ));

    println!("{}", pretty(reduce(term1))); // λx0. λx1. (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 (x0 x1)))))))))))))))))))))))))))
    println!("{}", pretty(reduce(term2))); // λx0. λx1. λx2. ((x0 x1) x2)
}

Expr {
  ty: A,
  kind: Block {
    body: Block {
      stmts: [ Stmt {
        kind: Expr {
          expr: Expr {
            ty: [closure@examples/ex.rs:6:1: 6:20],
            kind: Closure {
              closure_id: DefId(0:10 ~ ex[b70b]::f::{closure#0}),
              substs: Closure(
                [i8, extern "rust-call" fn((A, A)) -> bool, ()],
              ),
              upvars: [],
              movability: None,
            },
          },
        },
      }],
      expr: Some(Expr {
        ty: A,
        kind: Adt {
          adt_def: A,
          variant_index: 0,
          substs: [],
          fields: [
            FieldExpr {
              name: field[0],
              expr: Expr {
                ty: i32,
                kind: Binary {
                  op: Mul,
                  lhs: Expr {
                    ty: i32,
                    kind: Field {
                      lhs: Expr {
                        ty: A,
                        kind: VarRef {
                          id: HirId {
                            owner: DefId(0:9 ~ ex[b70b]::f),
                            local_id: 2,
                          },
                        },
                      },
                    },
                    name: field[0],
                  },
                  rhs: Expr {
                    ty: i32,
                    kind: Field {
                      lhs: Expr {
                        ty: A,
                        kind: VarRef {
                          id: HirId {
                            owner: DefId(0:9 ~ ex[b70b]::f),
                            local_id: 2,
                          },
                        },
                      },
                      name: field[0],
                    },
                  },
                },
              },
            },
          ],
          base: None,
        },
      }),
    },
  },
}

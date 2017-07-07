# Graph-coloring-problem-with-SAT-solver
Solve graph coloring problem with SAT solver that I created.

I separated this into two sub parts:

1. Convert graph coloring problem into CNF.
2. Implement SAT solver to solve satifiability of CNF.

Detailed specifications are given in two pdf forms respecitvely.


Notes:

1. Graph coloring problem is converted into satisfiability(SAT) problem and solved by using a SAT solver.
2. Graph coloring problem is disassembled into a number of constraints caused by any two adjacent nodes. These constraints are appropriately converted into conjunctive normal forms(CNF).
3. I applied the DPLL algorithm, an evolved form of backtracking algorithm, to SAT solvers by eagerly using unit propagation and pure literal elimination.
4. I improved the DPLL algorithm by giving the utmost priority to the most constrained and also high priority to the most constraining variable.

Coursework from UCLA CS161

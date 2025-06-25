//! Print truth tables for an example formula.

use prop_logic::{Formula, TruthTabled as _, Variable};

fn main() {
    let p = Variable::with_data(1, "p");
    let q = Variable::with_data(2, "q");
    let r = Variable::with_data(3, "r");
    let f = (Formula::from(p) | !q) & (Formula::from(q) ^ !r);

    println!("{f:#}");
    println!("{}", f.get_truth_table());
}

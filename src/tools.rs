use godot::prelude::{Gd, GodotClass, Inherits, Node};

/**
 * Collect all descendants of `root` that are (or inherit) `T`.
 */
pub fn collect_children<T>(root: Gd<Node>, is_recursive: bool) -> Vec<Gd<T>>
where
    T: GodotClass + Inherits<Node>,
{
    fn dfs<T>(node: Gd<Node>, out: &mut Vec<Gd<T>>, is_recursive: bool)
    where
        T: GodotClass + Inherits<Node>,
    {
        let count = node.get_child_count();
        for i in 0..count {
            if let Some(child) = node.get_child(i) {
                if let Ok(typed) = child.clone().try_cast::<T>() {
                    out.push(typed);
                }
                if is_recursive {
                    dfs::<T>(child, out, is_recursive);
                }
            }
        }
    }

    let mut out = Vec::new();
    dfs::<T>(root, &mut out, is_recursive);
    out
}
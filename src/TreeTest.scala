object TreeTest extends App {

	val tree = Branch(Leaf(1), Branch(Leaf(4), Leaf(7)))

	println(tree)
	println(Tree.size(tree))
	println(Tree.maximum(tree))
	println(Tree.depth(tree))
	println(Tree.map(tree)((a) => a*2))
}
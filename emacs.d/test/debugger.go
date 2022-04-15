package main

import "fmt"

type Int int

func (i *Int) add2() {
	*i += 2
}

func (i *Int) add3() {
	*i += 3
}

func (i *Int) sub7() {
	*i -= 7
}

func main() {
	var n Int = 0
	n.add2()
	n.add3()
	fmt.Println(n)
	n.add2()
	n.add3()
	fmt.Println(n)
	n.sub7()

	n.add2()
	n.add3()
	n.add2()
	n.add3()
	n.sub7()

	for i := 0; i < 10; i++ {
		n.add2()
		n.add2()
		n.add2()
		n.add2()
	}

	n.add2()
	n.add3()
	n.add2()
	n.add3()
	n.sub7()

	fmt.Printf("%d", n)
}

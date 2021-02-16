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
          *i -= 5
  }

  func main() {
          var n Int = 0
          n.add2()
          n.add3()
          n.add2()
          n.add3()
          n.sub7()

          n.add2()
          n.add3()
          n.add2()
          n.add3()
          n.sub7()

          n.add2()
          n.add3()
          n.add2()
          n.add3()
          n.sub7()


          fmt.Printf("%d", n)
  }

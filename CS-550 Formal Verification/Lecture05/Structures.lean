structure Point where
  x : Float
  y : Float

def origin : Point := { x := 0.0, y := 0.0}
#eval origin
#eval origin.x
#eval origin.y

def addPoints (p1 : Point) (p2 : Point) : Point :=
  { x := p1.x + p2.x, y := p1.y + p2.y }

#eval addPoints { x := 1.5, y := 32 } { x := -8, y := 0.2 }


def distance (p1: Point) (p2: Point) : Float :=
  Float.sqrt (((p2.x - p1.x) ^ 2.0) + ((p2.y - p1.y) ^ 2.0))

#eval distance { x := 1.0, y := 2.0 } { x := 5.0, y := -1.0 }


structure Point3D where
  x : Float
  y : Float
  z : Float
def origin3D : Point3D := { x := 0.0, y := 0.0, z := 0.0 }

#check { x := 0.0, y := 0.0 : Point}


-- Updating structures
def zeroX (p : Point) : Point :=
  { x := 0, y := p.y }

-- Does not modify existing values, it creates new values that share old fields.
def zeroX2 (p : Point) : Point :=
  { p with x := 0 }

def fourAndThree : Point :=
  { x := 4.3, y := 3.4 }


#eval fourAndThree
#eval zeroX2 fourAndThree
#eval fourAndThree


-- Constructors (not a clean Lean style)

#check Point.mk 1.5 2.8

#check (Point.mk)


-- Override the constructor names

structure NewPoint where
  point ::
  x : Float
  y : Float

#check (NewPoint.point)
#check (NewPoint.x)
#check (NewPoint.y)


#eval "one string".append " and another"


def Point.modifyBoth (f: Float -> Float) (p : Point) : Point :=
  { x := f p.x, y := f p.y}

/-
 Target of the accessor notation is used as the first argument in which
 the type matches, not necessarily the first argument.
-/
#eval fourAndThree.modifyBoth Float.floor


-- Exercices
structure RectangularPrism where
  height : Float
  width : Float
  depth : Float


def volume (rec : RectangularPrism) : Float :=
  rec.height * rec.width * rec.depth

#eval volume {height := 3, width := 4, depth := 5}


structure Segment where
  x : Float
  y : Float

def length (s : Segment) : Float :=
  Float.abs s.y - s.x


#eval length { x := 0.5 , y := 1}

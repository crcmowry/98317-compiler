(* Symbols *)
structure StringSymbol = SymbolFun (structure Value = StringHashable)
structure Symbol = StringSymbol
structure SymbolOrdered = SymbolOrderedFun(structure Symbol = Symbol)
structure SymbolHashable = SymbolHashableFun(structure Symbol = Symbol)

(* Hashtables *)
structure CharHashTable   = HashTable (structure Key = CharHashable)
structure IntHashTable    = HashTable (structure Key = IntHashable)
structure StringHashTable = HashTable (structure Key = StringHashable)
structure SymbolHashTable = HashTable (structure Key = SymbolHashable)
structure UnitHashTable   = HashTable (structure Key = UnitHashable)

(* Sets and dictionaries *)
structure CharListSet   = ListSet (structure Elem = CharOrdered)
structure IntListSet    = ListSet (structure Elem = IntOrdered)
structure IntInfListSet = ListSet (structure Elem = IntInfOrdered)
structure StringListSet = ListSet (structure Elem = StringOrdered)
structure SymbolListSet = ListSet (structure Elem = SymbolOrdered)
structure UnitListSet   = ListSet (structure Elem = UnitOrdered)

structure CharListDict   = ListDict (structure Key = CharOrdered)
structure IntListDict    = ListDict (structure Key = IntOrdered)
structure IntInfListDict = ListDict (structure Key = IntInfOrdered)
structure StringListDict = ListDict (structure Key = StringOrdered)
structure SymbolListDict = ListDict (structure Key = SymbolOrdered)
structure UnitListDict   = ListDict (structure Key = UnitOrdered)

structure CharRedBlackSet   = RedBlackSet (structure Elem = CharOrdered)
structure IntRedBlackSet    = RedBlackSet (structure Elem = IntOrdered)
structure IntInfRedBlackSet = RedBlackSet (structure Elem = IntInfOrdered)
structure StringRedBlackSet = RedBlackSet (structure Elem = StringOrdered)
structure SymbolRedBlackSet = RedBlackSet (structure Elem = SymbolOrdered)
structure UnitRedBlackSet   = RedBlackSet (structure Elem = UnitOrdered)

structure CharRedBlackDict   = RedBlackDict (structure Key = CharOrdered)
structure IntRedBlackDict    = RedBlackDict (structure Key = IntOrdered)
structure IntInfRedBlackDict = RedBlackDict (structure Key = IntInfOrdered)
structure StringRedBlackDict = RedBlackDict (structure Key = StringOrdered)
structure SymbolRedBlackDict = RedBlackDict (structure Key = SymbolOrdered)
structure UnitRedBlackDict   = RedBlackDict (structure Key = UnitOrdered)

structure CharSplaySet   = SplaySet (structure Elem = CharOrdered)
structure IntSplaySet    = SplaySet (structure Elem = IntOrdered)
structure IntInfSplaySet = SplaySet (structure Elem = IntInfOrdered)
structure StringSplaySet = SplaySet (structure Elem = StringOrdered)
structure SymbolSplaySet = SplaySet (structure Elem = SymbolOrdered)
structure UnitSplaySet   = SplaySet (structure Elem = UnitOrdered)

structure CharSplayDict   = SplayDict (structure Key = CharOrdered)
structure IntSplayDict    = SplayDict (structure Key = IntOrdered)
structure IntInfSplayDict = SplayDict (structure Key = IntInfOrdered)
structure StringSplayDict = SplayDict (structure Key = StringOrdered)
structure SymbolSplayDict = SplayDict (structure Key = SymbolOrdered)
structure UnitSplayDict   = SplayDict (structure Key = UnitOrdered)

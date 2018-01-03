Craft Calc
==========

Craft Calc is a Domain Specific Language (DSL) for **Calculating Materials Needed** when Crafting

When given a set of crafting rules (via editing the file named "rules.txt"), this program can calculate the materials needed for crafting your target (described in the file named "input.txt").

Demo
----

rules.txt
```
basic_materials: Wood, Coal

WoodenPlank = Wood
Stick = WoodenPlank * 2
Torch = Coal + Stick
```

input.txt
```
(Torch * 10) % basicMaterials
```

output
```
(Torch * 10) % basicMaterials
= Wood * 20 + Coal * 10
```

Run Demo
--------

```
ghc craft-calc.hs -o calc.exe
mv calc.exe demo1/
cd demo1
./calc.exe
```

Todo
----

- Fractional number support
- Multiple crafting path support
- Calculate the least cost and show the crafting plan
- Better alias support
- Macro support


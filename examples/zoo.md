In this example, the inventory of various Zoo animals from different
City Zoos is maintained in a `KVITable`, where each animal is
additionally classified by parameters such as Biome and Diet.

The `KVITable` creation is somewhat lengthy due to the size of the
inventory; the [SampleTables.hs](../test/SampleTables.hs) file should be
consulted for the definition of the `KVITable` `zooTable` and
`zooTable2`.

Rendering the data without any column stacking, but with multiple row
groupings:

```
import Data.KVITable.Render.ASCII

render (defaultRenderConfig
        { sortKeyVals = True
        , rowRepeat   = False
        , rowGroup = [ "Location", "Biome", "Category" ]
        })
       zooTable2
```

results in the following:

```
____ snip vv ____
|  Location |    Biome | Category |      Diet |    Name |     Subtype | Count |
+-----------+----------+----------+-----------+---------+-------------+-------+
|        LA |   Jungle |   Animal | Herbivore |   Hippo |             |     1 |
|           |----------+----------+-----------+---------+-------------+-------+
|           | Savannah |   Animal | Carnivore |    Lion |             |     4 |
|           |          |          | Herbivore | Giraffe |             |     2 |
|           |          |          |           |   Rhino |             |     3 |
+-----------+----------+----------+-----------+---------+-------------+-------+
|     Miami |    Polar |     Bird | Carnivore | Penguin |      Gentoo |    20 |
|           |----------+----------+-----------+---------+-------------+-------+
|           | Savannah |   Animal | Carnivore |    Lion |             |     2 |
|           |          |          | Herbivore | Giraffe | Reticulated |     3 |
+-----------+----------+----------+-----------+---------+-------------+-------+
|  New York | Savannah |   Animal | Carnivore |    Lion |             |     3 |
+-----------+----------+----------+-----------+---------+-------------+-------+
| San Diego |   Jungle |   Animal |  Omnivore |    Bear |         Sun |     1 |
|           |----------+----------+-----------+---------+-------------+-------+
|           |   Plains |   Animal |  Omnivore |    Bear |       Black |     1 |
|           |          |          |           |         |       Brown |     1 |
|           |----------+----------+-----------+---------+-------------+-------+
|           |    Polar |   Animal |  Omnivore |    Bear |       Polar |     1 |
|           |          |----------+-----------+---------+-------------+-------+
|           |          |     Bird | Carnivore | Penguin |     Emperor |     8 |
|           |          |          |           |         |      Gentoo |     2 |
|           |----------+----------+-----------+---------+-------------+-------+
|           | Savannah |   Animal | Carnivore |    Lion |             |     9 |
+-----------+----------+----------+-----------+---------+-------------+-------+
____ snip ^^ ____
```

## With Column Stacking

Removing the "Subtype" field from the zoo table and then modifying the
rendering configuration to add column stacking on the `"Name"` key
`Val`s results in:

```
____ snip vv ____
|  Location |    Biome | Category |      Diet | Bear | Giraffe | Hippo | Lion | Penguin | Rhino | <- Name
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
|        LA |   Jungle |   Animal | Herbivore |      |         |     1 |      |         |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           | Savannah |   Animal | Carnivore |      |         |       |    4 |         |       |
|           |          |          | Herbivore |      |       2 |       |      |         |     3 |
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
|     Miami |    Polar |     Bird | Carnivore |      |         |       |      |      20 |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           | Savannah |   Animal | Carnivore |      |         |       |    2 |         |       |
|           |          |          | Herbivore |      |       3 |       |      |         |       |
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
|  New York | Savannah |   Animal | Carnivore |      |         |       |    3 |         |       |
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
| San Diego |   Jungle |   Animal |  Omnivore |    1 |         |       |      |         |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           |   Plains |   Animal |  Omnivore |    2 |         |       |      |         |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           |    Polar |   Animal |  Omnivore |    1 |         |       |      |         |       |
|           |          |----------+-----------+------+---------+-------+------+---------+-------+
|           |          |     Bird | Carnivore |      |         |       |      |      10 |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           | Savannah |   Animal | Carnivore |      |         |       |    9 |         |       |
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
____ snip ^^ ____
```

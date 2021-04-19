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

or in HTML (with CSS inherited from the Markdown
configuration... imagine how much nicer this could be with your own
CSS styling!):

******

<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th class="kvitable_th">Location</th>
      <th class="kvitable_th">Biome</th>
      <th class="kvitable_th">Category</th>
      <th class="kvitable_th">Diet</th>
      <th class="kvitable_th">Name</th>
      <th class="kvitable_th">Subtype</th>
      <th class="kvitable_th">Count</th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=4>LA</th>
      <th class="kvitable_th last_in_group">Jungle</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Herbivore</th>
      <th class="kvitable_th last_in_group">Hippo</th>
      <th class="kvitable_th last_in_group"></th>
      <td class="kvitable_td last_in_group">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3>Savannah</th>
      <th class="kvitable_th last_in_group" rowspan=3>Animal</th>
      <th class="kvitable_th">Carnivore</th>
      <th class="kvitable_th">Lion</th>
      <th class="kvitable_th"></th>
      <td class="kvitable_td">4</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th" rowspan=2>Herbivore</th>
      <th class="kvitable_th">Giraffe</th>
      <th class="kvitable_th"></th>
      <td class="kvitable_td">2</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Rhino</th>
      <th class="kvitable_th last_in_group"></th>
      <td class="kvitable_td last_in_group">3</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3>Miami</th>
      <th class="kvitable_th last_in_group">Polar</th>
      <th class="kvitable_th last_in_group">Bird</th>
      <th class="kvitable_th last_in_group">Carnivore</th>
      <th class="kvitable_th last_in_group">Penguin</th>
      <th class="kvitable_th last_in_group">Gentoo</th>
      <td class="kvitable_td last_in_group">20</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2>Savannah</th>
      <th class="kvitable_th last_in_group" rowspan=2>Animal</th>
      <th class="kvitable_th">Carnivore</th>
      <th class="kvitable_th">Lion</th>
      <th class="kvitable_th"></th>
      <td class="kvitable_td">2</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Herbivore</th>
      <th class="kvitable_th last_in_group">Giraffe</th>
      <th class="kvitable_th last_in_group">Reticulated</th>
      <td class="kvitable_td last_in_group">3</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">New York</th>
      <th class="kvitable_th last_in_group">Savannah</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Carnivore</th>
      <th class="kvitable_th last_in_group">Lion</th>
      <th class="kvitable_th last_in_group"></th>
      <td class="kvitable_td last_in_group">3</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=7>San Diego</th>
      <th class="kvitable_th last_in_group">Jungle</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Omnivore</th>
      <th class="kvitable_th last_in_group">Bear</th>
      <th class="kvitable_th last_in_group">Sun</th>
      <td class="kvitable_td last_in_group">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2>Plains</th>
      <th class="kvitable_th last_in_group" rowspan=2>Animal</th>
      <th class="kvitable_th" rowspan=2>Omnivore</th>
      <th class="kvitable_th" rowspan=2>Bear</th>
      <th class="kvitable_th">Black</th>
      <td class="kvitable_td">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Brown</th>
      <td class="kvitable_td last_in_group">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3>Polar</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Omnivore</th>
      <th class="kvitable_th last_in_group">Bear</th>
      <th class="kvitable_th last_in_group">Polar</th>
      <td class="kvitable_td last_in_group">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2>Bird</th>
      <th class="kvitable_th" rowspan=2>Carnivore</th>
      <th class="kvitable_th" rowspan=2>Penguin</th>
      <th class="kvitable_th">Emperor</th>
      <td class="kvitable_td">8</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Gentoo</th>
      <td class="kvitable_td last_in_group">2</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Savannah</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Carnivore</th>
      <th class="kvitable_th last_in_group">Lion</th>
      <th class="kvitable_th last_in_group"></th>
      <td class="kvitable_td last_in_group">9</td>
    </tr>
  </tbody>
</table>

******

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

And as HTML:

******
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th class="kvitable_th">Location</th>
      <th class="kvitable_th">Biome</th>
      <th class="kvitable_th">Category</th>
      <th class="kvitable_th">Diet</th>
      <th class="kvitable_th">Bear</th>
      <th class="kvitable_th">Giraffe</th>
      <th class="kvitable_th">Hippo</th>
      <th class="kvitable_th">Lion</th>
      <th class="kvitable_th">Penguin</th>
      <th class="kvitable_th">Rhino</th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;Name</th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3>LA</th>
      <th class="kvitable_th last_in_group">Jungle</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Herbivore</th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">1</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2>Savannah</th>
      <th class="kvitable_th last_in_group" rowspan=2>Animal</th>
      <th class="kvitable_th">Carnivore</th>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">4</td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Herbivore</th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">2</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">3</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3>Miami</th>
      <th class="kvitable_th last_in_group">Polar</th>
      <th class="kvitable_th last_in_group">Bird</th>
      <th class="kvitable_th last_in_group">Carnivore</th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">20</td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2>Savannah</th>
      <th class="kvitable_th last_in_group" rowspan=2>Animal</th>
      <th class="kvitable_th">Carnivore</th>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">2</td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Herbivore</th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">3</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">New York</th>
      <th class="kvitable_th last_in_group">Savannah</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Carnivore</th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">3</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=5>San Diego</th>
      <th class="kvitable_th last_in_group">Jungle</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Omnivore</th>
      <td class="kvitable_td last_in_group">1</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Plains</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Omnivore</th>
      <td class="kvitable_td last_in_group">2</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2>Polar</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Omnivore</th>
      <td class="kvitable_td last_in_group">1</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Bird</th>
      <th class="kvitable_th last_in_group">Carnivore</th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">10</td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group">Savannah</th>
      <th class="kvitable_th last_in_group">Animal</th>
      <th class="kvitable_th last_in_group">Carnivore</th>
      <td class="kvitable_td last_in_group"></td>
      <td class="kvitable_td last_in_group"></td>
      <td class="kvitable_td last_in_group"></td>
      <td class="kvitable_td last_in_group">9</td>
      <td class="kvitable_td last_in_group"></td>
      <td class="kvitable_td last_in_group"></td>
    </tr>
  </tbody>
</table>

******
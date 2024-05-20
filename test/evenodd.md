When a maximum size is specified for the table, it will be truncated.

Here is the even/odd table in its unconstrained form, with column stacking at the thousands

****
| millions | thousands | _____ 0 _____ | _______ 1 ________ | _______ 2 ________ | <- hundreds
|          |           | _ 0 _ | _ 2 _ | _ 0 _ | ___ 2 ____ | _ 0 _ | ___ 2 ____ | <- tens
|          |           | 0 | 1 | 0 | 1 | 0 | 1 |    0 |   1 | 0 | 1 |    0 |   1 | <- ones
+----------+-----------+---+---+---+---+---+---+------+-----+---+---+------+-----+
|        0 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|        1 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|        2 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
****

****
<table class="kvitable">
<thead class="kvitable_head">
<tr class="kvitable_tr">
<th rowspan="3" class="kvitable_th">
<div>
<span>millions</span>
</div>
</th>
<th rowspan="3" class="kvitable_th">
<div>
<span>thousands</span>
</div>
</th>
<th colspan="4" class="kvitable_th multicol">
<div>
<span>0</span>
</div>
</th>
<th colspan="4" class="kvitable_th multicol">
<div>
<span>1</span>
</div>
</th>
<th colspan="4" class="kvitable_th multicol">
<div>
<span>2</span>
</div>
</th>
<th class="rightlabel kvitable_th">&nbsp;&larr;hundreds</th>
</tr>
<tr class="kvitable_tr">
<th colspan="2" class="kvitable_th multicol">
<div>
<span>0</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>2</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>0</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>2</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>0</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>2</span>
</div>
</th>
<th class="rightlabel kvitable_th">&nbsp;&larr;tens</th>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="rightlabel kvitable_th">&nbsp;&larr;ones</th>
</tr>
</thead>
<tbody class="kvitable_body">
<tr class="kvitable_tr">
<th rowspan="3" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="3" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="3" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
</tbody>
</table>
****


And here is the same table with a `maxCells` specification of `60`:

****
| millions | thousands | _____ 0 _____ | _______ 1 ________ | _______ 2 ________ | <- hundreds
|          |           | _ 0 _ | _ 2 _ | _ 0 _ | ___ 2 ____ | _ 0 _ | ___ 2 ____ | <- tens
|          |           | 0 | 1 | 0 | 1 | 0 | 1 |    0 |   1 | 0 | 1 |    0 |   1 | <- ones
+----------+-----------+---+---+---+---+---+---+------+-----+---+---+------+-----+
|        0 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|     {+2} |         0 |   |   |   |   |   |   |      |     |   |   |      |     |
|          |         1 |   |   |   |   |   |   |      |     |   |   |      |     |
|          |         2 |   |   |   |   |   |   |      |     |   |   |      |     |
****

****
<table class="kvitable">
<thead class="kvitable_head">
<tr class="kvitable_tr">
<th rowspan="3" class="kvitable_th">
<div>
<span>millions</span>
</div>
</th>
<th rowspan="3" class="kvitable_th">
<div>
<span>thousands</span>
</div>
</th>
<th colspan="4" class="kvitable_th multicol">
<div>
<span>0</span>
</div>
</th>
<th colspan="4" class="kvitable_th multicol">
<div>
<span>1</span>
</div>
</th>
<th colspan="4" class="kvitable_th multicol">
<div>
<span>2</span>
</div>
</th>
<th class="rightlabel kvitable_th">&nbsp;&larr;hundreds</th>
</tr>
<tr class="kvitable_tr">
<th colspan="2" class="kvitable_th multicol">
<div>
<span>0</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>2</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>0</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>2</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>0</span>
</div>
</th>
<th colspan="2" class="kvitable_th multicol">
<div>
<span>2</span>
</div>
</th>
<th class="rightlabel kvitable_th">&nbsp;&larr;tens</th>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th class="rightlabel kvitable_th">&nbsp;&larr;ones</th>
</tr>
</thead>
<tbody class="kvitable_body">
<tr class="kvitable_tr">
<th rowspan="3" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">even</td>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="3" class="kvitable_th">
<div>
<span>{+2}</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
<td class="kvitable_td">
</td>
</tr>
</tbody>
</table>
****

The same table with no column stacking and a `maxCells` specification of `60`:

****
| millions | thousands | hundreds | tens | ones | Value |
+----------+-----------+----------+------+------+-------+
|        0 |         0 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         1 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         2 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
****

****
<table class="kvitable">
<thead class="kvitable_head">
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>millions</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>thousands</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>hundreds</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>tens</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>ones</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>Value</span>
</div>
</th>
</tr>
</thead>
<tbody class="kvitable_body">
<tr class="kvitable_tr">
<th rowspan="36" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="12" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="4" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">even</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">even</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="12" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th rowspan="4" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">even</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">even</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="12" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th rowspan="4" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">even</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">even</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">odd</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="36" class="kvitable_th">
<div>
<span>{+2}</span>
</div>
</th>
<th rowspan="12" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="4" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="12" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th rowspan="4" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="12" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th rowspan="4" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="4" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th rowspan="2" class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th rowspan="2" class="kvitable_th">
<div>
<span>2</span>
</div>
</th>
<th class="kvitable_th">
<div>
<span>0</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
<tr class="kvitable_tr">
<th class="kvitable_th">
<div>
<span>1</span>
</div>
</th>
<td class="kvitable_td">
</td>
</tr>
</tbody>
</table>
****

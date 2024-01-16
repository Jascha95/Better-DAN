<?


$nosql = 1;

include "config.php";
include "lib/init.inc";
require("../include.php");

kopf(5);

$head = str_replace("__TITLE__", $lang['title'], $design['head']);

echo $head;

include "themes/".$theme."/header.inc";
?>

<table border=0 width=100%><tr><td>
<table border=0 width=<?echo $design['list_width']?> cellspacing=0 
cellpadding=<? echo $design['borderwidth']?>><tr>
<td bgcolor=<? echo $design['bordercolor'] ?>>
<table border=0 cellspacing=0 cellpadding=10 width=100%>
<? 
$count = 0;
if(!empty($forum_array)){
   while(list($x, ) = each($forum_array)){
      verbinden();
      $q = pg_query("select count(id) from articles where forum='$x'");
      $r=pg_fetch_assoc($q);
      if ($r["count"] == 1) {$pl="";} else {$pl="en";}
      $bgcolor=($count%2==0||$count==0?$design['evencolor']:$design['oddcolor']);
      echo "<tr><td bgcolor=\"$bgcolor\">".
         "<a class=forum_name href="http://proglang.informatik.uni-freiburg.de/teaching/sopra/2004/forum/\&quot;list.php?f=$x\&quot;">".
	 stripslashes($forum_array[$x]['name'])."</a> (".$r["count"]." Nachricht$pl)<table border=0><tr><td>";
      echo "&nbsp;</td><td>";
      echo stripslashes($forum_array[$x]['desc'])."&nbsp;</td></tr></table><br></td></tr>";
      $count++;
   }
}

?>
</table>
</td></tr></table>
<? include "inc/langs.inc" ?>
</td></tr></table>
<?
include "themes/".$theme."/footer.inc";

echo $design['footer'];

fuss();
?>

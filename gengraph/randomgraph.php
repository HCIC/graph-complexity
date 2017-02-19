<?
header('Access-Control-Allow-Origin: *');
header('Access-Control-Allow-Methods: GET, POST');
header("Access-Control-Allow-Headers: X-Requested-With");

passthru("bin/distrib -c -r 50 0 1 -1 2.2 | bin/graph");
?>

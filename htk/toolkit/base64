#!/usr/bin/perl -w

use MIME::Base64 ();

if (not open(FH, "< $ARGV[0]")) {
  print "Error: cannot open $ARGV[0]\n";
} else {

  local $/;
  $file = <FH>;
  close FH;

  $encoded = MIME::Base64::encode($file);
  
  if ($ARGV[1]) {
    
    if (not open(FH, "> $ARGV[1]")) {
      print "Error: cannot create $ARGV[1]\n";
    } else {
      print FH $encoded;
      print "$ARGV[1] created\n";
    }
    
  } else {
    print "Base64 encoding:\n\n";
    print $encoded . "\n";
  }
}

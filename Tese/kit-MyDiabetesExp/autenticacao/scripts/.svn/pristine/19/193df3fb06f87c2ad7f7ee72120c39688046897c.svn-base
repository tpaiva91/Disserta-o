#!/usr/bin/perl
# This file compiles a LaTeX  document
# into PDF format using pdflatex
# Pedro Brandão
use strict;
use POSIX;
use Cwd;
use Getopt::Long;

# Globals
# #######

my $TEXTEMPLATE_FILE = 'template.tex';
my $TEXTEMPLATE_SEP = "adddere";
my $TEXTEMPLATE_ENC = ':encoding(UTF-8)';
my $TEXTEMPLATE_BEF;
my $TEXTEMPLATE_AFTER;
my $USER_PASSWORD_LINE = "Password";
my $USER_USER_LINE = "Utilizador";

my $CDIR= getcwd();
if ( $^O =~ /WIN/i)
{
   $CDIR = "$CDIR\\";
}
else
{
   $CDIR = "$CDIR/";
}

usage() if ( !@ARGV );

########################
# Read template

my $bef = 1;
my $templateH;
open($templateH, "< $TEXTEMPLATE_ENC", $TEXTEMPLATE_FILE)
        || die "$0: can't open file for reading: $!";
while(<$templateH>){
   if (/$TEXTEMPLATE_SEP/){
      $bef =0;
      next;
   }
   if($bef){
      $TEXTEMPLATE_BEF .= $_;
   }
   else{
      $TEXTEMPLATE_AFTER .= $_;
   }
}
#cut \n from author line
$TEXTEMPLATE_BEF =~s/\n$//s ;

close($templateH);

foreach my $file (map {glob} @ARGV) {
    my $fh;
    open($fh,"<",$file) 
       || die "$0: can't open file for reading: $!";
    my $user;
    my $password;
    while(<$fh>) {
       s/\n//;
       if(/$USER_PASSWORD_LINE/){
           $password = $_;
       }
       elsif(/$USER_USER_LINE/) {
          $user = $_;
       }
    }
    close($fh);
    my $fileTex = $file . ".tex";
    printTEX($fileTex,$user,$password);
    system("pdflatex $fileTex")==0   || die ("pdflatex failed");
}
#---------------------------------------------------------------------
# Aux funcs

sub usage {
	print "Usage: $0 <files>\n";
	exit 0;
}
sub printTEX {
   my $filename=shift;
   my $u = shift;
   my $p = shift;
   my $fh;
    open($fh,"> $TEXTEMPLATE_ENC",$filename) 
       || die "$0: can't open file for writing $!";
   print $fh $TEXTEMPLATE_BEF;
   print $fh $u,"\\\\\n",$p;
   print $fh $TEXTEMPLATE_AFTER;
}


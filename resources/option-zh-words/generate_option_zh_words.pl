use strict;
use warnings;

open IN_FILE,"<option-zh-words.txt";
my $data=<IN_FILE>;
close IN_FILE;

#$data='a.垂死的,临终的vt.&vi.染,染色n.染料,染色n.责任,义务;职责,职务;税,关税a.多灰尘的,灰蒙蒙的;粉末状的;灰色的n.垃圾箱n.尘土;粉末vt.擦净,除尘;撒(粉末等)prep.在...的整个期间,在..期间的某个时候n.一种食品(用肉和蔬菜作馅,蒸熟来吃)a.迟钝的,呆笨的;单调的,枯躁的;阴暗的';
my @all_words;

my @all_meaning= split /([a-zA-Z]+\.([|&][a-zA-Z]+\.)*)/,$data;
my $i=1;#pass the first
while($i<@all_meaning){
	my ($attr,$pass,$meaning)=($all_meaning[$i],$all_meaning[$i + 1],$all_meaning[$i +2]);
	#print "meaning:$meaning attr:$attr\n";
	for (split /;/,$meaning){
		for my $word(split /,/,$_){
			push @all_words,remove_unwanted($word,$attr);	
		}
	}
	$i+=3;
}
print "@all_words\n";

sub remove_unwanted{
	my ($word,$attr)=@_;
	if($attr =~ /a[djv]*\./){
		my $word_len=length($word);
		my $ZH_CHAR_LEN=3;
		if(($word_len == 3*$ZH_CHAR_LEN) or ($word_len ==5*$ZH_CHAR_LEN)){		
			$word =~ s/(地)|(的)$//;
		}
	}
	return $word;
}

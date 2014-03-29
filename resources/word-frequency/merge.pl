use strict;
use warnings;

open WORD_RANK_FILE,"<word-rank-by-frequency2.txt";
my @lines=<WORD_RANK_FILE>;
close WORD_RANK_FILE;

my @words;
for my $line(@lines){
	next if $line =~ /^\s*#/;
	next if $line =~ /^\s*$/;
	my @data=split /\s+/,$line;
	die "error line:$line\n" unless (scalar @data)>=3;
	die "error not found word. line:$line\n" unless $data[1] =~ /^[\w\.]+$/;
	push @words,$data[1];
}

open EN_TO_ZH_DICT,"<../dict/en-zh.txt";
my @dict_lines=<EN_TO_ZH_DICT>;
close EN_TO_ZH_DICT;

my %en2zh_dict;
for my $dict_line(@dict_lines){
	next if $dict_line =~ /^\s*#/;
	next if $dict_line =~ /^\s*$/;
	my @data=split /\s+/,$dict_line;
	die "error line:$dict_line\n" unless (scalar @data)==2;
	die "error not found word. line:$dict_line\n" unless $data[0] =~ /^[\w\.]+$/;
	$en2zh_dict{$data[0]}=$data[1];
}

my @word_meanings;
for my $word(@words){
	if(exists $en2zh_dict{$word}){
		push @word_meanings,[$word,$en2zh_dict{$word}];
	}elsif($word =~/^(\w+?)s$/ ){
		my $word_removed_s=$1;
		if(exists $en2zh_dict{$word_removed_s}){
			push @word_meanings,[$word_removed_s,$en2zh_dict{$word_removed_s}];
		}
	}elsif($word =~/^(\w+?)es$/ ){
		my $word_removed_es=$1;
		if(exists $en2zh_dict{$word_removed_es}){
			push @word_meanings,[$word_removed_es,$en2zh_dict{$word_removed_es}];
		}
	}elsif($word =~/^(\w+?)ed$/ ){
		my $word_removed_ed=$1;
		if(exists $en2zh_dict{$word_removed_ed}){
			push @word_meanings,[$word_removed_ed,$en2zh_dict{$word_removed_ed}];
		}
	}elsif($word =~/^(\w+?e)d$/ ){
		my $word_removed_d=$1;
		if(exists $en2zh_dict{$word_removed_d}){
			push @word_meanings,[$word_removed_d,$en2zh_dict{$word_removed_d}];
		}
	}elsif($word =~/^(\w+?)ing$/ ){
		my $word_removed_ing=$1;
		if(exists $en2zh_dict{$word_removed_ing}){
			push @word_meanings,[$word_removed_ing,$en2zh_dict{$word_removed_ing}];
		}
	}elsif($word =~/^(\w+?)ly$/ ){
		my $word_removed_ly=$1;
		if(exists $en2zh_dict{$word_removed_ly}){
			push @word_meanings,[$word_removed_ly,$en2zh_dict{$word_removed_ly}];
		}
	}else{
		warn "word not found in dict:$word\n";
	}
}

open WORD_RANK_DICT,">word-meaning-rank-by-frequency2.txt";
for my $word_meaning(@word_meanings){
	print WORD_RANK_DICT $word_meaning->[0],"\t",$word_meaning->[1],"\n";
}
close WORD_RANK_DICT;

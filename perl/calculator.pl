#!/usr/bin/perl
use strict;
use warnings;

my %variables;

sub evaluate {
    my ($expression) = @_;
    $expression =~ s/^\s+|\s+$//g; 

    if ($expression =~ /^([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.+)$/) {
        my ($var, $value) = ($1, $2);
        my $result = evaluate($value);
        if (defined $result) {
            $variables{$var} = $result;
            return $result;
        } else {
            return undef; 
        }
    } elsif ($expression =~ /^([a-zA-Z_][a-zA-Z0-9_]*)$/) {
        my $var = $1;
        if (exists $variables{$var}) {
            return $variables{$var};
        } else {
            print "Hata: '$var' değişkeni tanımlı değil.\n";
            return undef;
        }
    } elsif ($expression =~ /^\s*(-?\d+(\.\d+)?)\s*$/) {
        return $1;
    } elsif ($expression =~ /^\s*\((.+)\)\s*$/) {
        return evaluate($1);
    } elsif ($expression =~ /^\s*(.+)\s*([\+\-\*\/])\s*(.+)\s*$/) {
        my ($left, $op, $right) = ($1, $2, $3);
        my $left_val = evaluate($left);
        my $right_val = evaluate($right);

        if (defined $left_val && defined $right_val) {
            if ($op eq '+') {
                return $left_val + $right_val;
            } elsif ($op eq '-') {
                return $left_val - $right_val;
            } elsif ($op eq '*') {
                return $left_val * $right_val;
            } elsif ($op eq '/') {
                if ($right_val == 0) {
                    print "Hata: Sıfıra bölme.\n";
                    return undef;
                }
                return $left_val / $right_val;
            } else {
                print "Hata: Geçersiz operatör '$op'.\n";
                return undef;
            }
        } else {
            return undef;
        }
    } else {
        print "Hata: Geçersiz ifade '$expression'.\n";
        return undef;
    }
}

sub run_calculator {
    while (1) {
        print "> ";
        my $input = <STDIN>;
        chomp $input;

        last if $input =~ /^exit$/i;

        my $result = evaluate($input);
        if (defined $result) {
            print "$result\n";
        }
    }
}

run_calculator();
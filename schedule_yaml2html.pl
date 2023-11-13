#!/usr/bin/env perl

use v5.32;
use utf8;

use YAML::XS qw(LoadFile);
use Carp::Assert;
use Try::Tiny;
use Carp;
use open qw(:std :encoding(UTF-8));

assert(@ARGV >= 1, 'first argument should be input yaml filename');
my $sched = LoadFile($ARGV[0]);
my @schedule;
try{
    @schedule = @{$sched};
}
catch{
    croak 'schedule should be a yaml array';
};
# print html table header
say <<'EOF';
    <table style="width:100%;">
        <colgroup>
            <col style="width: 1%">
            <col style="width: 2%">
            <col style="width: 95%">
        </colgroup>
        <thead>
            <tr class="header">
                <th>Speaker</th>
                <th>Time ∧ Location</th>
                <th>Paper</th>
            </tr>
            <tr class="odd">
                <th></th>
                <th colspan="2">Abstract</th>
            </tr>
        </thead>
        <tbody>
EOF
for (@schedule){
    my @event;
    try{
        @event = @{$_};
    }
    catch{
        croak 'each element of the top-level array should be another array';
    };
    assert(@event == 5, 'each event should be a 5-tuple of (Speaker, Time ∧ Location, Paper Title, Link to Paper, Abstract)');
    say <<"EOF";
        <tr class="odd">
            <td>$event[0]</td>
            <td>$event[1]</td>
            <td><a href="$event[3]">$event[2]</a></td>
        </tr>
        <tr class="even">
            <td></td>
            <td colspan="2">$event[4]</td>
        </tr>
EOF
}
# print rest of html table
say <<"EOF";
        </tbody>
    </table>
EOF

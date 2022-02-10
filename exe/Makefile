.SUFFIXES: .log .sft .hs
.PHONY: default recordAll record index clean

TEAMS=$(notdir $(wildcard teams/*))

default: recordAll index

recordAll:
    for t1 in $(TEAMS); do for t2 in $(TEAMS); do make -s TEAM1=$$t1 TEAM2=$$t2 record; done; done

index: tapes/index.html

tapes/index.html: tapes/*.log
    ./mkIndex.sh > $@

clean:
    rm -f tapes/*
    rm -f teams/*/*.o teams/*/*.hi

# the rest of the file requires TEAM1 and TEAM2 to be defined

record: teams/$(TEAM1) teams/$(TEAM2) tapes/$(TEAM1)-$(TEAM2).log

tapes/$(TEAM1)-$(TEAM2).log: teams/$(TEAM1)/* teams/$(TEAM2)/*
    ulimit -t 15; sfRecord teams/$(TEAM1) teams/$(TEAM2) > $@ 2>&1
    [ -f $(TEAM1)-$(TEAM2).sft ] && mv $(TEAM1)-$(TEAM2).sft tapes

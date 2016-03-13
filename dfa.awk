# ex 2.9

BEGIN {
	# (aba)+	(action 1);
	# (a(b*)a)	(action 2);
	# (a|b)		(action 3);

	edges[1,"a"] = 2
	edges[1,"b"] = 4
	edges[2,"a"] = 3
	edges[2,"b"] = 5
	edges[5,"a"] = 6
	edges[5,"b"] = 9
	edges[6,"a"] = 7
	edges[7,"b"] = 8
	edges[8,"a"] = 6
	edges[9,"a"] = 10
	edges[9,"b"] = 9

	final[2] = 3
	final[3] = 2
	final[4] = 3
	final[6] = 1
	final[10] = 2
}

{
	inpos = 0
	printf("lf\tcurr\t|\tinpos\tlfpos\tcurrpos\taction\n")
	printf("--\t----\t|\t-----\t-----\t-------\t------\n")
	while (inpos < length($0)) {
		gettok()
		printf("--\t----\t|\t-----\t-----\t-------\t------\n")
	}
}

function gettok(lastfinal, lastfinalpos, curr, currpos) {
	lastfinal = 0
	lastfinalpos = 0
	curr = 1
	currpos = inpos

	printf("%d\t%d\t|\t%d\t%d\t%d\n", lastfinal, curr, inpos, lastfinalpos, currpos)
	for (;;) {
		currpos++
		curr = edges[curr,substr($0,currpos,1)]
		if (curr in final) {
			lastfinal = curr
			lastfinalpos = currpos
		}
		if (curr == 0) {
			printf("%d\t%d\t|\t%d\t%d\t%d\t%d\n", lastfinal, curr, inpos, lastfinalpos, currpos, final[lastfinal])
			inpos = lastfinalpos
			return
		} else {
			printf("%d\t%d\t|\t%d\t%d\t%d\n", lastfinal, curr, inpos, lastfinalpos, currpos)
	 	}
	}
}

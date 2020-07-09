test:
	elm-test --watch

bench:
	${BROWSER} http://localhost:8000 &
	cd benchmark && elm-live src/StatisticsBench.elm

compile:
	cd benchmark && elm make src/StatisticsBench.elm

test:
	elm-test --watch

bench:
	${BROWSER} http://localhost:8000 &
	cd benchmark && elm-live src/StatisticsBench.elm

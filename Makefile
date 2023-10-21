clean:
	lein clean; rm -rf figwheel_server.log

serve:
	lein figwheel

gen:
	lein run -m pequod-plus.gen ppex001 > ppex001.cljs

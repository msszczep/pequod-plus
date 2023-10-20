clean:
	lein clean; rm -rf figwheel_server.log

serve:
	lein figwheel

irssi_notifier() {
	ssh christopher@cxa.la 'echo -n "" > ~/.irssi/fnotify; tail -f ~/.irssi/fnotify' | \
		while read heading message; do
			notify-send "IRC" "$heading: $message"
		done
}

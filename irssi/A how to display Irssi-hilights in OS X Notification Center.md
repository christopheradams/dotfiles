# Irssi in Mac OS X Notification Center

![Mou icon](http://earthlingsoft.net/ssp/blog/graphics/TerminalIcon.png)

## Overview

This guide will explain how you can make irc messages in a screen on a remote server appear in your [Mac OS X Lion Notification Center](http://support.apple.com/kb/HT5362) with the help of [terminal-notifier](https://github.com/alloy/terminal-notifier). 

We will also explain how the process can be automatically started each time you log in to your Mac and ensure the connection to the server is kept alive. 

While this is a guide on how to make it work with irssi you can imagine all the possibilties this opens up! You could for instance set up a task on your server that logs certain error messages from your server log files etc. 

### Table of Contents

* [Requirements](https://gist.github.com/prebenlm/5562656#requirements)
* [Part 1: Writing messages to file on a remote server](https://gist.github.com/prebenlm/5562656#part-1-writing-messages-to-file-on-a-remote-server)
* [Part 2: Installing terminal-notifier on your Mac](https://gist.github.com/prebenlm/5562656#part-2-installing-terminal-notifier-on-your-mac)
* [Part 3: Read messages from server and send notifications](https://gist.github.com/prebenlm/5562656#part-3-read-messages-from-server-and-send-notifications)
* [Part 4: Start task when logging in](https://gist.github.com/prebenlm/5562656#part-4-start-task-when-logging-in)
* [Resources that helped me](https://gist.github.com/prebenlm/5562656#resources-that-helped-me)
* [Questions and suggestions for improvements](https://gist.github.com/prebenlm/5562656#questions-and-suggestions-for-improvements)


### Requirements

We assume that you have the following knowledge and tools:

* Mac OS X 10.8 or greater
* Remote server running a [screen](http://www.gnu.org/software/screen/manual/screen.html) session with [irssi](http://www.irssi.org/documentation)
* You use an ssh key to connect to the remote server (For Part 4 atleast)
* Basic knowledge on the above terms and programs including [Terminal](http://guides.macrumors.com/Terminal)

[*\(Go to Top\)*](https://gist.github.com/prebenlm/5562656#file-a-how-to-display-irssi-hilights-in-os-x-notification-center-md)

---

### Part 1: Writing messages to file on a remote server

The first thing we need is a script for irssi that will write what we want to display in the notification center to a file on the remote server. 

We'll use [fnotify](https://gist.github.com/matthutchinson/542141) in this walktrough, but feel free to create your own script that will retrive what you want it to retrieve.

fnotify was authored by Thorsten Leemhuis and released under the [GNU General Public License](http://www.gnu.org/licenses/gpl.html).

1. Connect to your remote server
2. Go to ~/.irssi/scripts (Create any missing folders)
3. Create the file fnotify.pl
4. Copy and paste the code from <https://gist.github.com/matthutchinson/542141> into the file
5. Continue irssi from an already existing screen session or start irssi
6. In irssi, load your script by typing `/script load fnotify.pl`


#### Testing and use
Irssi should now begin to write private messages and hilights you receive to the log file. If you didn't change the code this should be saved to the file ~/.irssi/fnotify

Ensure this is working by having another irc user hilight you or send you a private message and then open /.irssi/fnotify to see if the message was written to it.

By default it's likely that your IRC username trigger hilights, but you can also add other terms it should match. You can do this with /hilight. Please type `/help hilight` in irssi for more details. 

If you ever create your own script remember that you'll have to reload the script in irssi each time you have saved changes before irssi will execute the updates. You can do this simply by repeating the command `/script load fnotify.pl`.

[*\(Go to Top\)*](https://gist.github.com/prebenlm/5562656#file-a-how-to-display-irssi-hilights-in-os-x-notification-center-md) 

---

### Part 2: Installing terminal-notifier on your Mac

[terminal-notifier](https://github.com/alloy/terminal-notifier) is a very useful tool that makes it easy to trigger OS X Notifications from Terminal and scripts. 

The [official readme](https://github.com/alloy/terminal-notifier#readme) from the developer should be good enough to show you how to install and use it. 

The next parts assume however that you have installed the [ruby-version](https://github.com/alloy/terminal-notifier/tree/master/Ruby) as detailed below. [Ruby on your Mac](http://net.tutsplus.com/tutorials/ruby/how-to-install-ruby-on-a-mac/) will be required

1. Type `gem install terminal-notifier` in Terminal


#### Testing and use

Once the ruby-version is installed you can test it by typing the following in Terminal. 

    terminal-notifier -title 'Title' -message 'This is a simple test message.' -activate com.apple.Terminal
	

This should cause a notification message to appear on your screen and you will see the confirmation message `* Notification delivered.` in your Terminal window. If you click on the notification box you will activate Terminal. 

[*\(Go to Top\)*](https://gist.github.com/prebenlm/5562656#file-a-how-to-display-irssi-hilights-in-os-x-notification-center-md) 

---

### Part 3: Read messages from server and send notifications

Now you need to create a script on your Mac that will read from the file on the server in _"real time"_ and then send the messages to the Mac OS X Notification Center with terminal-notifier. 

Below we have provided you with a basic function you can use and experiment with. This script displays the messages but will also look for links. If it finds a link it uses the -open flag in terminal-notifier to make it so the url opens in your browser if you click on the notification message. When there is no link it will activate the Terminal-window instead.


```bash
irssi_notifier() {
	ssh user@remoteserver.com 'echo -n "" > ~/.irssi/fnotify; tail -f ~/.irssi/fnotify' | \
        	while read heading message; do
			url=`echo \"$message\" | grep -Eo 'https?://[^ >]+' | head -1`;

			if [ ! "$url" ]; then
				terminal-notifier -title "\"$heading\"" -message "\"$message\"" -activate com.apple.Terminal;
			else
				terminal-notifier -title "\"$heading\"" -message "\"$message\"" -open "\"$url\"";
			fi;
		done
	}
```

Please note that you will need to change `ssh user@remoteserver.com` in the code above to your correct information. 

1. Open or create the file ~/.bash_profile
2. Copy and paste the code above into ~/.bash_profile
3. Fix the ssh-details in the script and save
4. Reload .bash_profile by typing `. ~/.bash_profile` in Terminal


#### Testing, use and solving ssh-problem

When typing `irssi_notifier` in Terminal you should now be able to manully run the process. The process will not end until the script either fails, you close the Terminal-window or you send a [sigkill-signal](http://en.wikipedia.org/wiki/Unix_signal#SIGKILL). 

In some cases you may experience that the script seems to silently fail or that it fails after a certain amount of time. You may notice this by seeing hilights in your irssi, but having received no notifications on your Mac. This is usually due to connection timing out without your ssh client being made aware of it. What you need to do is to send "a request" to the server about keeping your connection to the server alive. 

In order to fix this problem you need to edit either one of these files: /etc/ssh/ssh_config or ~/.ssh/config and add this on the first line: ```ServerAliveInterval 180``` . You can read more on these links: [1](http://ocaoimh.ie/2008/12/10/how-to-fix-ssh-timeout-problems/), [2](http://nileshbansal.blogspot.no/2007/02/prevent-timeouts-in-ssh.html)

Once again you should ensure irssi is writing hilights and private messages to the log file on the server. If you receive a hilight in irssi on the server while the function irssi_notifier is running on your local machine, you should see the Mac OS X notification popping up shortly thereafter.

When you do changes to .bash_profile you'll always need to reload it before the changes will have any effect.

[*\(Go to Top\)*](https://gist.github.com/prebenlm/5562656#file-a-how-to-display-irssi-hilights-in-os-x-notification-center-md) 

---

### Part 4: Start task when logging in 

It would not feel very intuitive if we had to check on whether the task was running at all times. Having to start it manually every time we logged in wouldn't be a lot of fun either.

There is a brilliant way to make sure it's always running and that it starts automatically when we log in. 


1. Create the file ~/irssi_notifier_run.sh
2. You need to give the system permission to execute/run the file, so do `chmod a+x ~/irssi_notifier_run.sh`
3. Paste the code below into it: 
```bash
#!/bin/sh
source ~/.bash_profile; irssi_notifier;
```
		
4. Go to ~/Library/LaunchAgents
5. Create the file com.prebenlm.irssi_notifier.plist
6. Paste the following into the file: 

```xml
<?xml version="1.0" encoding="UTF-8"?>
	<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
	<plist version="1.0">
		<dict>
			<key>Label</key>
			<string>com.identifier.irssi_notifier</string>
			<key>Program</key>
			<string>/Users/user/irssi_notifier_run.sh</string>
			<key>KeepAlive</key>
			<true/>
		</dict>
	</plist>
```

There are two things you can change here: 

* com.identifier.irssi_notifier this is the normal "naming convention". The file should start with com. Then the next bit is an "identifier". Apple for instance use apple and Valve use valvesoftware. It's like a company / author name. irrsi_notifier is the script / action name here.
* /Users/user/irssi_notifier_run.sh you'll need to change from "user" to your username and double check rest of the path.



7. To add this task to the user account, type this in Terminal: `launchctl load ~/Library/LaunchAgents/com.identifier.irssi_notifier.plist` (Remember to fix the path if necessary) 



#### Testing and use

If you log out and log in you should still receive notifications about new hilights and private messages. The best thing is that you do not need to do anything more to make it happen!

If you have [protected your ssh key](https://help.github.com/articles/working-with-ssh-key-passphrases) (used for logging into your server without your server password) with a pass phrase ssh-agent will be triggered on startup by the task irssi_notifier. You will then be prompted for the ssh pass phrase before it can start.

If it doesn't seem to be working, you can check for errors related to the task by tailing the system log `tail -f /var/log/system.log`

[*\(Go to Top\)*](https://gist.github.com/prebenlm/5562656#file-a-how-to-display-irssi-hilights-in-os-x-notification-center-md) 

---

### Resources that helped me

I would've been unable to figure all of this out on my own. I really appreciate all the resources I've been through in order to make this work like I envisioned. 

osxdaily.com had this very helpful guide on terminal-notifier: <http://osxdaily.com/2012/08/03/send-an-alert-to-notification-center-from-the-command-line-in-os-x/>

matthewhutchinson.net had a guide on how to combine irssi, screen, fnotify and [Growl](http://growl.info/): <http://matthewhutchinson.net/2010/8/21/irssi-screen-fnotify-and-growl-on-osx>

spatof.org wrote a function that I built upon: <http://www.spatof.org/zshaolin/misc/im/irc.html>

[Kent](http://stackoverflow.com/users/164835/kent) on stackoverflow.com was very helpful when I needed assistance to retrieve url from string <http://stackoverflow.com/a/16502803/1015149>

[trisweb](http://stackoverflow.com/users/835539/trisweb) on stackoverflow.com had a very good reply on a question about tasks that run in the background on startup <http://stackoverflow.com/a/13372744/1015149>

Both [nileshbansal](http://ocaoimh.ie/2008/12/10/how-to-fix-ssh-timeout-problems/) and [http://ocaoimh.ie/](http://nileshbansal.blogspot.no/2007/02/prevent-timeouts-in-ssh.html) for providing information on the ssh fix. Originally I had another source for this, but forgot to write about it before.

---

### Questions and suggestions for improvements

Please do not hesitate to contact me if you find anything that is unclear or you are having trouble following any of the steps. I would also be happy if someone wishes to contribute to this guide. 

[*\(Go to Top\)*](https://gist.github.com/prebenlm/5562656#file-a-how-to-display-irssi-hilights-in-os-x-notification-center-md) 
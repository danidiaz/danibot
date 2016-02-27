LINKS
=====

https://api.slack.com/bot-users

https://api.slack.com/methods

https://api.slack.com/rtm

Check the info about bot users [here](https://api.slack.com/docs/oauth).

[Blog post about creating Slack bots](https://medium.com/slack-developer-blog/slack-bot-onboarding-3b4c979de374).

[Securing API access tokens](http://developer.securekey.com/securing-api-access-tokens/)

[Strategy for keeping secret info such as API keys out of source control?](http://programmers.stackexchange.com/questions/205606/strategy-for-keeping-secret-info-such-as-api-keys-out-of-source-control)

QUOTES
======

> To begin a RTM session make an authenticated call to the rtm.start API method.
> This provides an initial set of team metadata and a message server WebSocket
> URL. Once you have connected to the message server it will provide a stream of
> events, including both messages and updates to the current state of the team.
> This allows a client to easily maintain a synchronized local copy of all team
> data and messages.

EXAMPLES OF BUILDING SLACK BOTS
===============================

https://scotch.io/tutorials/building-a-slack-bot-with-node-js-and-chuck-norris-super-powers

http://nordicapis.com/building-an-intelligent-bot-using-the-slack-api/

https://docs.apitools.com/blog/2015/01/15/a-slack-bot-without-a-server.html

http://blog.somewhatabstract.com/2015/03/02/writing-a-simple-slack-bot-with-node-slack-client/

http://blog.simontimms.com/2015/06/07/building-a-simple-slack-bot/

NOTES
=====

- Use the bot API token when opening the connection with rtm.start.

- Bots seem to start on the "direct message" channel, but admins can invite
  them into other channels.

- The messages exchanged in the main API and the rtm seems to have elements in
  common.

- Most entities have both name and identity fields. IM channel are the exception: they have an identity but not a name. The reason, probably, is that IM channels appear differently to each participant. 

- The message_changed and message_deleted events do not have a "text" field as the top level.

- "<@U0MKNPE9Y>: the message text".

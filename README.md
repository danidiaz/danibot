A very basic framework for creating Slack bots.

The bot will respond to messages in IM channels, and also to  messages
explicitly directed at him in general channels (`@nameofthebot: some message`).

To create your bot, import `Network.Danibot.Main` and pass a handler function
of type `Text -> IO Text` to `defaultMain`. 

The executable takes as argument the path to a json file containing the Slack
API token, with format

    {
        "slack_api_token" : "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    }


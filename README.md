A very basic framework for creating Slack bots.

The bot will respond to messages in IM channels, and also to  messages
explicitly directed at him in general channels (`@nameofthebot: some message`).

The bot reads the Slack api token from the environment variable
"DANIBOT_SLACK_API_TOKEN".

The default danibot executable comes with a few example handlers:

- *up? host port* 

  Checks if a port is open in a host.

- *lookup key* 

  Checks the value of key in a dictionary that is loaded at
  startup with the --dict parameter. The dictionary is a json object with
  string values.

- *help* 

  Lists available options.

To create your own customized bot, import `Network.Danibot.Main` and pass a
value of type `IO (Either String (Text -> IO Text))` to `mainWith`, where `Text
-> IO Text` is the type of the handler function.


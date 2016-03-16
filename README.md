A very basic framework for creating Slack bots.

The bot will respond to messages in IM channels, and also to  messages
explicitly directed at him in general channels (`@nameofthebot: some message`).

To create your bot, import `Network.Danibot.Main` and pass a value of type `IO
(Either String (Text -> IO Text))` to `mainWith`, where `Text -> IO Text` is
the type of the handler function.


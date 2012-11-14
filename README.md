simple-log-syslog
=================

Syslog backend for simple-log

<pre>
yourFunction :: MonadLog m => m ()
yourFunction = scope "your" $ log Trace "Hello"

run :: IO ()
run = do
    l &lt;- newLog (fileCfg "log.cfg" 60) [syslog "name"]
    withLog l yourFunction
</pre>

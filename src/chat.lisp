(in-package :charm)

;;;; Commands that implement player-to-player chat.

(defparameter *max-channels* 20)

(defun avatar-in-channel (avatar channel)
  (member channel (channels avatar) :test #'string-equal))

(defun is-valid-channel-name (name)
  (cl-ppcre:scan "^[\\w-]{3,12}$" name))

(defcommand (actor ("chat" "/") :word channel :rest message)
  "Sends a message to a chat channel. The message will be seen by all players
  who have joined the channel."
  (if (avatar-in-channel actor channel)
      (let ((text (merge-tokens message)))
        (maphash-values #'(lambda (session)
                            (with-slots (avatar) session
                              (when (and avatar (avatar-in-channel avatar channel))
                                (send-client-command session "showChat"
                                                     (string-downcase channel)
                                                     (describe-brief actor :capitalize t)
                                                     text))))
                        *sessions*))
      (show-text actor
                 "You are not in the ~s channel. See `help:channel` for more information."
                 channel)))

(defcommand (actor "channel" :word subcommand :word channel)
  "This command displays information about chat channels, or lets you join or leave a channel.

  When *subcommand* is not specified, the command displays a list of those chat
  channels you have joined. Otherwise, *subcommand* can be one of the following:

  - 'who' will display a list of players in *channel*.

  - 'join' will cause you to join *channel*.

  - 'leave' will cause you to leave *channel*.

  Note that you can be a member of at most 20 channels at any time.
  "
  (cond
    ((not subcommand)
     (show-text actor "You are a member of the following chat channels: ~a."
                (format-list (sort (channels actor) #'string<) :conjunction nil)))
    ((not (member subcommand '("who" "join" "leave") :test #'string-equal))
     (show-text actor "Invalid subcommand ~(~s~). See `help:channel` for more information."
                subcommand))
    ((not channel)
     (show-text actor "Specify the name of the channel you want to use with the ~(~s~) subcommand."
                subcommand))
    ((string-equal subcommand "who")
     (let ((members (keep-if #'(lambda (session)
                                 (with-slots (avatar) session
                                   (when (and avatar (avatar-in-channel avatar channel))
                                     (describe-brief avatar))))
                             (hash-table-values *sessions*))))
       (show-text actor "~a." (format-list members))))
    ((string-equal subcommand "leave")
     (if (avatar-in-channel actor channel)
         (progn
           (deletef (channels actor) channel :test #'string-equal)
           (show-text actor "You have left the ~(~s~) channel." channel))
         (show-text actor "You are not in the ~(~s~) channel." channel)))
    ((string-equal subcommand "join")
     (cond
       ((>= (length (channels actor)) *max-channels*)
        (show-text actor "You have already joined the maximum allowed number of channels."))
       ((not (is-valid-channel-name channel))
        (show-text actor "~(~s~) is not a valid channel name." channel))
       ((avatar-in-channel actor channel)
        (show-text actor "You are already in the ~(~s~) channel." channel))
       (t
        (push channel (channels actor))
        (show-text actor "You have joined the ~(~s~) channel." channel))))))

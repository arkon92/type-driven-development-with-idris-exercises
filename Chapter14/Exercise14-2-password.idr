data Access = LoggedOut | LoggedIn
data PwdCheck = Correct | Incorrect

data ShellCmd : (ty : Type) -> Access -> (ty -> Access) -> Type where
     Password : String -> ShellCmd PwdCheck LoggedOut
                                   (\check => case check of
                                                   Correct => LoggedIn
                                                   Incorrect => LoggedOut)
     Logout : ShellCmd () LoggedIn (const LoggedOut)
     GetSecret : ShellCmd String LoggedIn (const LoggedIn)

     PutStr : String -> ShellCmd () state (const state)
     Pure : (res : ty) -> ShellCmd ty (state_fn res) state_fn
     (>>=) : ShellCmd a state1 state2_fn ->
             ((res : a) -> ShellCmd b (state2_fn res) state3_fn) ->
             ShellCmd b state1 state3_fn

session : ShellCmd () LoggedOut (const LoggedOut)
session = do Correct <- Password "wurzel"
                | Incorrect => PutStr "Wrong password"
             msg <- GetSecret
             PutStr ("Secret code: " ++ show msg ++ "\n")
             Logout

{-
badSession : ShellCmd () LoggedOut (const LoggedOut)
badSession = do Password "wurzel"
                msg <- GetSecret
                PutStr ("Secret code: " ++ show msg ++ "\n")
                Logout
noLogout : ShellCmd () LoggedOut (const LoggedOut)
noLogout = do Correct <- Password "wurzel"
                 | Incorrect => PutStr "Wrong password"
              msg <- GetSecret
              PutStr ("Secret code: " ++ show msg ++ "\n")
Exercise14-2.idr:28:24-32:
   |
28 |                 msg <- GetSecret
   |                        ~~~~~~~~~
When checking right hand side of badSession with expected type
        ShellCmd () LoggedOut (const LoggedOut)

When checking an application of constructor Main.>>=:
        Type mismatch between
                ShellCmd String LoggedIn (const LoggedIn) (Type of GetSecret)
        and
                ShellCmd String (case _ of   Correct => LoggedIn Incorrect => LoggedOut) (\value => LoggedIn) (Expected type)

        Specifically:
                Type mismatch between
                        LoggedIn
                and
                        case _ of
                          Correct => LoggedIn
                          Incorrect => LoggedOut

Exercise14-2.idr:34:22-30:
   |
34 |               msg <- GetSecret
   |                      ~~~~~~~~~
When checking right hand side of Main.case block in noLogout at Exercise14-2.idr:32:26-42 with expected type
        ShellCmd () (case Correct of   Correct => LoggedIn Incorrect => LoggedOut) (\value => LoggedOut)

When checking an application of constructor Main.>>=:
        Type mismatch between
                ShellCmd () state (const state) (Type of PutStr _)
        and
                ShellCmd () (const LoggedIn msg) (\value => LoggedOut) (Expected type)

        Specifically:
                Type mismatch between
                        LoggedIn
                and
                        LoggedOut
-}
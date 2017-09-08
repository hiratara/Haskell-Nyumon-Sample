import Debug.Trace (trace) -- デバッグ用のモジュールをインポート。

f x = trace "f" $ x ^ 2
g x = trace "g" $ x - 1

main = let x = f 10
           y = g x
        in print y

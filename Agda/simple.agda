module simple where

trivial-lemma : { A B C : Set } → (A → B → C) -> (A → B) → A → C
trivial-lemma f g x = f x (g x)
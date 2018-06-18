Variable Prism : Type -> Type -> Type.

Variable preview : forall {S A : Type} (p : Prism S A) (s : S), option A.
Variable review : forall {S A : Type} (p : Prism S A) (a : A), S.

Definition LawP1 (S A : Type) (p : Prism S A) : Type :=
 forall (a : A), preview p (review p a) = Some a.

Definition LawP2 (S A : Type) (p : Prism S A) : Type :=
  forall (s : S) (a : A),
    preview p s = Some a -> review p a = s.

Definition PrismInj (S A : Type) (p : Prism S A) : Type :=
  forall (x y : A),
    review p x = review p y -> x = y.

Lemma lemmaPrismInj (S A : Type) (p : Prism S A) : LawP1 S A p -> PrismInj S A p.
Proof.
  unfold PrismInj. unfold LawP1.
  intros lawp1 x y H.
  assert (Some x = Some y).
  rewrite <- (lawp1 x).
  rewrite <- (lawp1 y).
  f_equal. exact H.
  injection H0. auto. Qed.


Variable Lens : Type -> Type -> Type.

Variable view : forall {S A : Type} (l : Lens S A) (s : S), A.
Variable set : forall {S A : Type} (l : Lens S A) (a : A) (s : S), S.

Definition LawL1 (S A : Type) (l : Lens S A) : Type :=
  forall (s : S) (a : A),   
    view l (set l a s) = a.

Definition LawL2 (S A : Type) (l : Lens S A) : Type :=
  forall (s : S), set l (view l s) s = s.

Definition LensSurj (S A : Type) (l : Lens S A) : Type :=
  forall (a : A), exists (s : S), view l s = a.

Lemma lemmaLensSurj (S A : Type) (l : Lens S A) (s : S) :
  LawL1 S A l -> LensSurj S A l.
Proof.
  unfold LawL1. unfold LensSurj. intros lawl1 a.
  exists (set l a s).
  apply lawl1.
  Qed.

functor MapFn (K : ORD_KEY) 
= SplayMapFn(K) :> ORD_MAP where type Key.ord_key = K.ord_key;

-- /*****************************************************************************

                        -- Copyright (c) 2013 
                        
                        -- Aleksei Pupyshev, 
                        -- Eugene Zhurin,
                        
                        -- "Création de l'esprit" company
                        -- "Brain and Trauma Foundation" Switzerland

-- ******************************************************************************/
import Data.List
import Data.Ord

sort_stro :: (a, Float) -> (a, Float) -> Ordering
sort_stro (_, m1) (_, m2)
  | m1 < m2 = GT
  | m1 > m2 = LT
  | m1 == m2 = EQ
 
maxtri :: Eq a => ((Float, Float, Float), [(a, Float)]) ->
	((Float, Float, Float), [(a, Float)]) -> ((Float, Float, Float), [(a, Float)])
maxtri ((d1, r1, s1), st1) ((d2, r2, s2), st2)
	| oldlsnw == True = ((d2, r2, s2), st2)
	| otherwise = ((d1, r1, s1), st1)
	where oldlsnw = oldlsnwf ((d1, r1, s1), st1) ((d2, r2, s2), st2)

oldlsnwf :: ((Float, Float, Float), [(a, Float)]) -> ((Float, Float, Float), [(a, Float)]) -> Bool
oldlsnwf ((d1, r1, s1), _) ((d2, r2, s2), _) = ((d1/s1) + (r1/(s1^2))) < ((d2/s2) + (r2/(s2^2)))

(/\) :: Eq a => [(a, a, Float)] -> (a, Float) -> [(a, a, Float)]
(/\) ls el = [x | x <- ls, x /=/ el]

(/=/) :: Eq a => (a, a, Float) -> (a, Float) -> Bool
(/=/) (li, lk, _) (oj, _) = (li /= oj) && (lk /= oj)

mrmri :: Eq a =>((Float, Float, Float), [(a, Float)]) -> [(a, a, Float)] -> 
	(a, Float) -> ((Float, Float, Float), [(a, Float)])
mrmri ((d, r, s), setf) lf (oxi, doi) = ((d2, r2, s2), setf2)
	where 
		d2 = d + doi
		r2 = r + (sum [rs | (i, k, rs)<- lf, not ((i, k, rs) /=/ (oxi, doi))])
		s2 = s + 1
		setf2 = ((oxi, doi):setf)
		
mrmrn3 :: Eq a =>((Float, Float, Float), [(a, Float)]) 
	-> [(a, a, Float)] -> [(a, Float)] -> ((Float, Float, Float), [(a, Float)])
mrmrn3 ((d, r, s), setf) lf (ox:oxs) = if oldlsnw
	then maxtri thismr (mrmrn3 ((d, r, s), setf) (lf/\ox) oxs)
	else ((d, r, s), setf)
	where 
		thismr = mrmri ((d, r, s), setf) lf ox
		oldlsnw = oldlsnwf ((d, r, s), setf) thismr
mrmrn3 ((d, r, s), setf) _ [] = ((d, r, s), setf)

mutifl :: Ord a => ([a] -> [a] -> Float) -> [[a]] -> [(Int, Int, Float)]
mutifl f dtlst = [(i, k, (f (dtlst!!fromIntegral(i)) (dtlst!!fromIntegral(k)))) | i <- [0..n], k <- [0..n], i<k]
	where n = length dtlst

mutiol :: (Ord a , Num a) => ([a] -> [a] -> Float) -> [[a]] -> [a] -> a -> [(a, Float)]
mutiol _ [] _ _ = []
mutiol f (x:xs) outs n = (n, f x outs):(mutiol f xs outs (n+1))

mrmrdiscr ::(Ord a , Num a)=> ([a] -> [a] -> Float) -> [[a]]-> [a] -> [(Int, Float)]
mrmrdiscr f dtlst outs = setout
	where 
		mutiolsort = sortBy sort_stro (mutiol f dtlst outs 0)
		setf = take 2 mutiolsort
		oxoxs = drop 2 mutiolsort
		lf = ((mutifl f dtlst)/\(setf!!0))/\(setf!!1)
		z = fromIntegral(0) :: Float
		setout = snd ( mrmrn3 ((z, z, z), setf) lf oxoxs)

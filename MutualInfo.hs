-- /*****************************************************************************

                        -- Copyright (c) 2013 
                        
                        -- Aleksei Pupyshev, 
                        -- Eugene Zhurin,
                        
                        -- "Création de l'esprit" company
			-- "Brain and Trauma Foundation" Switzerland

-- ******************************************************************************/

import Data.List
import Data.Ord

stru_crt :: [a] -> [a] -> [(a, Float, a, Float, Float)]
stru_crt [] _ = []
stru_crt (x:xs) (y:ys) = [(x, 0.0, y, 0.0, 0.0)] ++ stru_crt xs ys

s_f :: (a, Float, a, Float, Float) -> (a, Float)
s_f (x, p, _, _, _) = (x, p)

s_s :: (a, Float, a, Float, Float) -> (a, Float)
s_s (_, _, x, p, _) = (x, p)

sort_stru :: Ord a =>((a, Float, a, Float, Float) -> (a, Float)) ->
		(a, Float, a, Float, Float) -> (a, Float, a, Float, Float) -> Ordering
sort_stru f (x1,p1, y1, q1, pq1) (x2,p2, y2, q2, pq2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | a1 == a2 = EQ
  where 
	a1 = fst (f (x1, p1, y1, q1, pq1))
	a2 = fst (f (x2, p2, y2, q2, pq1))
	
diffb :: Eq a => ((a, Float, a, Float, Float) -> (a, Float)) -> [(a, Float, a, Float, Float)] -> [Bool]
diffb f (x0:x1:xs) = [(fst (f x0)) /= (fst (f x1))] ++ diffb f (x1:xs)
diffb f _ = []

s_freq :: [Integer] -> [Bool] -> [Integer]
s_freq fr [] = reverse fr
s_freq (fr:frs) (x:xs) = if x 
	then s_freq (1:fr:frs) xs 
	else s_freq ((fr+1):frs) xs
	
s_prob :: [Bool]-> [Float]
s_prob x = map ((/leng).fromIntegral) (s_freq [1] x) 
	where leng = fromIntegral(1+(length x)) :: Float

s_pr_list :: [Bool]-> [Float] -> [Float]
s_pr_list [] (pr:_) = [pr]
s_pr_list (x:xs) (pr:prs) = if x 
	then [pr] ++ s_pr_list xs prs
	else [pr] ++ s_pr_list xs (pr:prs)
	

fs_i :: Integer -> Float -> 
		(a, Float, a, Float, Float) -> (a, Float, a, Float, Float)
fs_i 1 pr (x, p, y, q, pq) = (x, pr, y, q, pq)
fs_i 2 pr (x, p, y, q, pq) = (x, p, y, pr, pq)
fs_i 3 pr (x, p, y, q, pq) = (x, p, y, q, pr)

s_cnv_pr :: Integer -> [Float] -> [(a, Float, a, Float, Float)] -> [(a, Float, a, Float, Float)]
s_cnv_pr n prs feat = zipWith (fs_i n) prs feat

s_entropy :: (a, Float, a, Float, Float) -> Float -> Float
s_entropy (x, p, y, q, pq) sm = sm + (pq)*((log pq) - (log p) - (log q))

s_mutinfo :: [(a, Float, a, Float, Float)] -> Float
s_mutinfo stru  = foldr s_entropy 0.0 stru



mutInfoOnlogn :: Ord a => Eq a => [a] -> [a] -> Float
mutInfoOnlogn ls1 ls2 = s_mutinfo stru_pr2
    where
        sort1_s = sortBy (sort_stru s_f) (stru_crt ls1 ls2)
        sort1_s_diff1 = diffb s_f sort1_s
        sort1_s_diff12 = zipWith (max) sort1_s_diff1 (diffb s_s sort1_s)
        dif1pro = s_prob sort1_s_diff1
        prolist1 = s_pr_list sort1_s_diff1 dif1pro
        stru_pr1 = s_cnv_pr 1 prolist1 sort1_s
        dif12pro = s_prob sort1_s_diff12
        prolist12 = s_pr_list sort1_s_diff12 dif12pro
        stru_pr12 = s_cnv_pr 3 prolist12 stru_pr1
        sort_stru_pr12 = sortBy (sort_stru s_s) stru_pr12
        sort2_s_diff2 = diffb s_s sort_stru_pr12
        dif2pro = s_prob sort2_s_diff2
        prolist2 = s_pr_list sort2_s_diff2 dif2pro
        stru_pr2 = s_cnv_pr 2 prolist2 sort_stru_pr12

  {-Chuong trinh co tuong-}
module AI where
-- Mau sac cua quan co D den, T trang, suy dien nos thuoc cac lop (Eq, Ord, Show, Read, Bounded, Enum)
data Maus = D | T 
	deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Loaiq, cac loai quan: tuongs, sy, tuongj, xe, phao, ma, tot.
data Loaiq = Ts | Sy | Tj | Xe | Ph | Ma | To
	deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Cacs hang duoc dang so tu 0..9, cacs cot duoc danh so tu 1..9.
-- Cac gia tri cua hang
data Hang = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9
	deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Cac gia tri cua cot
data Cot = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
	deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Quan, mieu ta 1 quan co cu the
type Quan = (Loaiq,Maus)
quan :: Loaiq -> Maus -> Quan
quan lq ms = (lq,ms)
-- Toado, mieu ta 1 vi tri tren ban co
type Toado = (Hang,Cot)
toado :: Hang -> Cot -> Toado
toado h c = (h,c)
hang = [H0 , H1 , H2 , H3 , H4 , H5 , H6 , H7 , H8 , H9]
cot = [C1 , C2 , C3 , C4 , C5 , C6 ,C7 , C8 , C9]
toadoAll = concat [zip (take 9 (repeat x)) cot | x <- hang]
-- Để mô tả việc đặt 1 quân lên bàn cờ ta dùng Kiểu Qutd, với quy ước quân đen bên H0, Quân trắng bên H9
type Qutd = (Quan,Toado)
qutd :: Loaiq -> Maus -> Hang -> Cot -> Qutd
qutd lq ms h c = (quan lq ms , toado h c)
-- Giá trị của kiểu dữ liệu QuTd có 1 số ràng buộc về vị trí của các quân(Ts,Sy,Tj,To)
luatVitri :: Qutd -> Bool
luatVitri ((lq,ms),(h,c)) = case lq of {Ts -> case ms of{D -> case h of{H0 -> case c of{C4 -> True;
																						C5 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		H1 -> case c of{C4 -> True;
																						C5 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		H2 -> case c of{C4 -> True;
																						C5 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		_  -> False;
																		};
														 T -> case h of{H7 -> case c of{C4 -> True;
																						C5 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		H8 -> case c of{C4 -> True;
																						C5 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		H9 -> case c of{C4 -> True;
																						C5 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		_  -> False;
																		};
														};
										Sy -> case ms of{D -> case h of{H0 -> case c of{C4 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		H1 -> case c of{C5 -> True;
																						_  -> False;
																						};
																		H2 -> case c of{C4 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		_  -> False;
																		};
														 T -> case h of{H7 -> case c of{C4 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		H8 -> case c of{C5 -> True;
																						_  -> False;
																						};
																		H9 -> case c of{C4 -> True;
																						C6 -> True;
																						_  -> False;
																						};
																		_  -> False;
																		};
														};
										Tj -> case ms of{D -> case h of{H0 -> case c of{C3 -> True;
																						C7 -> True;
																						_  -> False;
																						};
																		H2 -> case c of{C1 -> True;
																						C5 -> True;
																						C9 -> True;
																						_  -> False;
																						};
																		H4 -> case c of{C3 -> True;
																						C7 -> True;
																						_  -> False;
																						};
																		_  -> False;
																		};
														 T -> case h of{H5 -> case c of{C3 -> True;
																						C7 -> True;
																						_  -> False;
																						};
																		H7 -> case c of{C1 -> True;
																						C5 -> True;
																						C9 -> True;
																						_  -> False;
																						};
																		H9 -> case c of{C3 -> True;
																						C7 -> True;
																						_  -> False;
																						};
																		_  -> False;
																		};
														};
										To -> case ms of{D -> case h of{H0 -> case c of{_  -> False;
																						};
																		H1 -> case c of{_  -> False;
																						};
																		H2 -> case c of{_  -> False;
																						};
																		H3 -> case c of{C1 -> True;
																						C3 -> True;
																						C5 -> True;
																						C7 -> True;
																						C9 -> True;
																						_  -> False;
																						};
																		H4 -> case c of{C1 -> True;
																						C3 -> True;
																						C5 -> True;
																						C7 -> True;
																						C9 -> True;
																						_  -> False;
																						};
																		_  -> True;
																		};
														 T -> case h of{H7 -> case c of{_  -> False;
																						};
																		H8 -> case c of{_  -> False;
																						};
																		H9 -> case c of{_  -> False;
																						};
																		H5 -> case c of{C1 -> True;
																						C3 -> True;
																						C5 -> True;
																						C7 -> True;
																						C9 -> True;
																						_  -> False;
																						};
																		H6 -> case c of{C1 -> True;
																						C3 -> True;
																						C5 -> True;
																						C7 -> True;
																						C9 -> True;
																						_  -> False;
																						};
																		_  -> True;
																		};
														};
										_ -> True;
									  };

{- luat vi tri viet lai khong dung bieu thuc case
luatVitri :: Qutd -> Bool
luatVitri ((Ts,ms),(h,c)) = ham3 ms h c where  {ham3 D h c = ham2 h c where {ham2 H0 c = ham1 c where {ham1 C4 = True;
																							           ham1 C5 = True;
																							           ham1 C6 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H1 c = ham1 c where {ham1 C4 = True;
																							           ham1 C5 = True;
																							           ham1 C6 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H2 c = ham1 c where {ham1 C4 = True;
																							           ham1 C5 = True;
																							           ham1 C6 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 _ _  = False;
																	        };
											    ham3 T h c = ham2 h c where {ham2 H7 c = ham1 c where {ham1 C4 = True;
																									   ham1 C5 = True;
																							           ham1 C6 = True;
																							           ham1  _ = False;
																							           };
																			 ham2 H8 c = ham1 c where{ham1 C4 = True;
																									  ham1 C5 = True;
																									  ham1 C6 = True;
																									  ham1  _ = False;
																									  };
																			 ham2 H9 c = ham1 c where{ham1 C4 = True;
																									  ham1 C5 = True;
																									  ham1 C6 = True;
																									  ham1  _ = False;
																									 };
																			 ham2 _ _  = False;
																			};
												};
luatVitri ((Sy,ms),(h,c)) = ham3 ms h c where  {ham3 D h c = ham2 h c where {ham2 H0 c = ham1 c where {ham1 C4 = True;
																							           ham1 C6 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H1 c = ham1 c where {ham1 C5 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H2 c = ham1 c where {ham1 C4 = True;
																							           ham1 C6 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 _ _  = False;
																	        };
											    ham3 T h c = ham2 h c where {ham2 H7 c = ham1 c where {ham1 C4 = True;
																							           ham1 C6 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H8 c = ham1 c where {ham1 C5 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H9 c = ham1 c where {ham1 C4 = True;
																							           ham1 C6 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 _ _  = False;
																	        };
												};
luatVitri ((Tj,ms),(h,c)) = ham3 ms h c where  {ham3 D h c = ham2 h c where {ham2 H0 c = ham1 c where {ham1 C3 = True;
																							           ham1 C7 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H2 c = ham1 c where {ham1 C1 = True;
																									   ham1 C5 = True;
																									   ham1 C9 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H4 c = ham1 c where {ham1 C3 = True;
																							           ham1 C7 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 _ _  = False;
																	        };
											    ham3 T h c = ham2 h c where {ham2 H5 c = ham1 c where {ham1 C3 = True;
																							           ham1 C7 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H7 c = ham1 c where {ham1 C1 = True;
																									   ham1 C5 = True;
																									   ham1 C9 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 H9 c = ham1 c where {ham1 C3 = True;
																							           ham1 C7 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 _ _  = False;
																	        };
												};
luatVitri ((To,ms),(h,c)) = ham3 ms h c where  {ham3 D h c = ham2 h c where {ham2 H0 c = ham1 c where {ham1  _ = False;
																							          };
																	         ham2 H1 c = ham1 c where {ham1  _ = False;
																							          };
																	         ham2 H2 c = ham1 c where {ham1  _ = False;
																							          };
																			 ham2 H3 c = ham1 c where {ham1 C1 = True;
																									   ham1 C3 = True;
																									   ham1 C5 = True;
																									   ham1 C7 = True;
																									   ham1 C9 = True;
																							           ham1  _ = False;
																							          };
																			 ham2 H4 c = ham1 c where {ham1 C1 = True;
																									   ham1 C3 = True;
																									   ham1 C5 = True;
																									   ham1 C7 = True;
																									   ham1 C9 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 _ _  = True;
																	        };
											    ham3 T h c = ham2 h c where {ham2 H7 c = ham1 c where {ham1  _ = False;
																							          };
																	         ham2 H8 c = ham1 c where {ham1  _ = False;
																							          };
																	         ham2 H9 c = ham1 c where {ham1  _ = False;
																							          };
																			 ham2 H5 c = ham1 c where {ham1 C1 = True;
																									   ham1 C3 = True;
																									   ham1 C5 = True;
																									   ham1 C7 = True;
																									   ham1 C9 = True;
																							           ham1  _ = False;
																							          };
																			 ham2 H6 c = ham1 c where {ham1 C1 = True;
																									   ham1 C3 = True;
																									   ham1 C5 = True;
																									   ham1 C7 = True;
																									   ham1 C9 = True;
																							           ham1  _ = False;
																							          };
																	         ham2 _ _  = True;
																	        };
												};
luatVitri ((_,_),(_,_)) = True
-}
type Luotdi = Maus
doiluotdi :: Luotdi -> Luotdi
doiluotdi D = T
doiluotdi T = D
type Theco = ([Qutd],Luotdi)
luotdi :: Theco -> Luotdi
luotdi tc = snd tc
dsQutd :: Theco -> [Qutd]
dsQutd tc = fst tc
vanmoi :: Luotdi -> Theco
vanmoi ld = ([ ((Ts,D),(H0,C5)),((Sy,D),(H0,C4)),((Sy,D),(H0,C6)),((Tj,D),(H0,C3)),((Tj,D),(H0,C7))
              ,((Xe,D),(H0,C1)),((Xe,D),(H0,C9)),((Ph,D),(H2,C2)),((Ph,D),(H2,C8)),((Ma,D),(H0,C2)),((Ma,D),(H0,C8))
			  ,((To,D),(H3,C1)),((To,D),(H3,C3)),((To,D),(H3,C5)),((To,D),(H3,C7)),((To,D),(H3,C9))
			  ,((Ts,T),(H9,C5)),((Sy,T),(H9,C4)),((Sy,T),(H9,C6)),((Tj,T),(H9,C3)),((Tj,T),(H9,C7))
			  ,((Xe,T),(H9,C1)),((Xe,T),(H9,C9)),((Ph,T),(H7,C2)),((Ph,T),(H7,C8)),((Ma,T),(H9,C2)),((Ma,T),(H9,C8))
			  ,((To,T),(H6,C1)),((To,T),(H6,C3)),((To,T),(H6,C5)),((To,T),(H6,C7)),((To,T),(H6,C9)) ],ld)
dsqutdLuotdi :: Theco -> [Qutd]
dsqutdLuotdi tc = filter (\((_,ms),(_,_)) -> if ms == (luotdi tc) then True else False) (dsQutd tc)
dsqutdLuotdi' :: Theco -> [Qutd]
dsqutdLuotdi' tc = filter (\((_,ms),(_,_)) -> if ms /= (luotdi tc) then True else False) (dsQutd tc)
vitriTrong , vitriQuanLuotdi , vitriQuanLuotdi' :: Toado -> Theco -> Bool
vitriTrong (h,c) tc = not (elem (h,c) (snd (unzip (dsQutd tc))))
vitriQuanLuotdi (h,c) tc = elem (h,c) (snd (unzip (dsqutdLuotdi tc)))
vitriQuanLuotdi' (h,c) tc = elem (h,c) (snd (unzip (dsqutdLuotdi' tc)))
--Kiem tra 1 the co co dung luat vi tri khong
luatVitriok :: Theco -> Bool
luatVitriok tc = and (map luatVitri (dsQutd tc))
-- mo ta 1 quan co di chuyen
data Move = Move Qutd Toado
	deriving (Eq, Show)
move :: Qutd -> Toado -> Move
move qutd td = Move qutd td
tdMove :: Move -> Toado
tdMove (Move qutd td) = td
qutdMove :: Move -> Qutd
qutdMove (Move qutd td) = qutd
qutdMove' :: Move -> Qutd
qutdMove' m = (fst (qutdMove m), tdMove m)
--2 ham succ va pred trong modul chuan chi lam viec voi du lieu dang * 
--de tinh toan voi gia tri vuot bien ta dinh ngia 2 ham tang va gian lam viec voi du lieu dang * -> *

-- dinh nghia 1 so ham voi hang va cot
tien1, lui1, tien2, lui2 :: Hang -> Maybe Hang
tien1 h = if h == H9 then Nothing else Just (succ h)
lui1 h = if h == H0 then Nothing else Just (pred h)
tien2 h = if (h == H8) || (h == H9) then Nothing else Just (succ (succ h)) 
lui2 h = if (h == H0) || (h == H1) then Nothing else Just (pred (pred h)) 
phai1, trai1, phai2, trai2 :: Cot -> Maybe Cot
phai1 c = if c == C9 then Nothing else Just (succ c)
trai1 c = if c == C1 then Nothing else Just (pred c)
phai2 c = if (c == C8) || (c == C9) then Nothing else Just (succ (succ c)) 
trai2 c = if (c == C1) || (c == C2) then Nothing else Just (pred (pred c))
-- Luat di chuyen quan
luatMove :: Move -> Bool
luatMove (Move ((lq,ms),(h,c)) (h',c')) = case (lq,ms) of {(Ts,_) -> lmTs (h,c) (h',c');
													       (Sy,_) -> lmSy (h,c) (h',c');
													       (Tj,_) -> lmTj (h,c) (h',c');
													       (Xe,_) -> lmXe (h,c) (h',c');
													       (Ph,_) -> lmPh (h,c) (h',c');
													       (Ma,_) -> lmMa (h,c) (h',c');
													       (To,D) -> lmToD (h,c) (h',c');
						 								   (To,T) -> lmToT (h,c) (h',c');
														   }
    where lmTs (h,c) (h',c') | (tien1 h == Just h') && (c == c') = True
							 | (lui1 h == Just h') && (c == c') = True
							 | (phai1 c == Just c') && (h == h') = True
							 | (trai1 c == Just c') && (h == h') = True
							 | otherwise = False
							 ;
		  lmSy (h,c) (h',c') | (tien1 h == Just h') && (phai1 c == Just c') = True
							 | (lui1 h == Just h') && (phai1 c == Just c') = True
							 | (tien1 h == Just h') && (trai1 c == Just c') = True
							 | (lui1 h == Just h') && (trai1 c == Just c') = True
							 | otherwise = False
							 ;							 
		  lmTj (h,c) (h',c') | (tien2 h == Just h') && (phai2 c == Just c') = True
							 | (lui2 h == Just h') && (phai2 c == Just c') = True
							 | (tien2 h == Just h') && (trai2 c == Just c') = True
							 | (lui2 h == Just h') && (trai2 c == Just c') = True
							 | otherwise = False
							 ;
		  lmXe (h,c) (h',c') | (h == h') && (c /= c') = True
							 | (h /= h') && (c == c') = True
							 | otherwise = False
							 ;
		  lmPh (h,c) (h',c') | (h == h') && (c /= c') = True
							 | (h /= h') && (c == c') = True
							 | otherwise = False
							 ;
		  lmMa (h,c) (h',c') | (tien2 h == Just h') && (phai1 c == Just c') = True
							 | (tien2 h == Just h') && (trai1 c == Just c') = True
							 | (lui2 h == Just h') && (phai1 c == Just c') = True
							 | (lui2 h == Just h') && (trai1 c == Just c') = True
							 | (tien1 h == Just h') && (phai2 c == Just c') = True
							 | (tien1 h == Just h') && (trai2 c == Just c') = True
							 | (lui1 h == Just h') && (phai2 c == Just c') = True
							 | (lui1 h == Just h') && (trai2 c == Just c') = True
							 | otherwise = False
							 ;
		  lmToD (h,c) (h',c') | (tien1 h == Just h') && (c == c') = True
							  | (phai1 c == Just c') && (h == h') = True
							  | (trai1 c == Just c') && (h == h') = True
							  | otherwise = False
							  ;
		  lmToT (h,c) (h',c') | (lui1 h == Just h') && (c == c') = True
							  | (phai1 c == Just c') && (h == h') = True
							  | (trai1 c == Just c') && (h == h') = True
							  | otherwise = False
							  ;
-- loc ra 1 danh sach cacs nuoc di cos the co cua 1 quan tren ban co, thoa man ca luatVitri va luatMove
dsVitricothedi :: Qutd -> [Move]
dsVitricothedi qutd@(qu,td) = if luatVitri qutd then filter luatMove (map (move qutd) [x | x <- toadoAll, luatVitri (qu,x)]) else []						  
-- nuocDi nhan vao 1 move va 1 the co tra lai 1 the co 
nuocDi :: Move -> Theco -> Theco
nuocDi m@(Move _ (h',c')) tc@(_,ld) = ((qutdMove' m) : (filter (\((lq,ms),(h,c)) -> if ((h == h') && (c == c')) || (((lq,ms),(h,c)) == qutdMove m) then False else True) (dsQutd tc)) , doiluotdi ld )
	
-- kiem tra cac nuoc di cua xe phao									   
dskhoang :: Toado -> Toado -> [Toado]
dskhoang (h,c) (h',c') | (tien1 h < Just h') && (c == c') && (tien1 h /= Nothing) = (succ h,c) : dskhoang (succ h,c) (h',c')
					   | (lui1 h > Just h') && (c == c') && (lui1 h /= Nothing) = (pred h,c) : dskhoang (pred h,c) (h',c')
					   | (phai1 c < Just c') && (h == h') && (phai1 c /= Nothing) = (h,succ c) : dskhoang (h,succ c) (h',c')
					   | (trai1 c > Just c') && (h == h') && (trai1 c /= Nothing) = (h,pred c) : dskhoang (h,pred c) (h',c')
					   | otherwise = []
soquantrongdskhoang :: [Toado] -> Theco -> Int
soquantrongdskhoang dsk tc = length [x | x <- dsk , x `elem` (snd (unzip (dsQutd tc)))]
--ket thuc van co nhan vao 1 the co tra lai bool (ben den luot di ma mat tuong thi ket thuc van co)
ketThuc :: Theco -> Bool
ketThuc tc = [x | x <- dsQutd tc , (fst x) == (Ts,luotdi tc)] == [] 
--kolomattuong nhan vao 1 the co kiem tra xem co lo mat tuong khong
kolomattuong :: Theco -> Bool
kolomattuong tc = let dstdTs = [snd x | x <- dsQutd tc , ((fst x) == (Ts,D)) || ((fst x) == (Ts,T))] 
					in case (snd (head dstdTs)) /= (snd (last dstdTs)) of 
							True -> True ;
							False -> if soquantrongdskhoang (dskhoang (head dstdTs) (last dstdTs)) tc /= 0 then True else False
-- luat nuoc di nhan vao 1 Move va 1 Theco kiem tra xem nuoc di co hop le hay khong.
luatNuocdi :: Move -> Theco -> Bool
luatNuocdi m@(Move ((lq,ms),(h,c)) (h',c')) tc
	| ketThuc tc = False
	| kolomattuong tc = 
	                  if 
	                   (ms /= (luotdi tc)) 
					  then False 
					  else case lq of {Ts -> (not (vitriQuanLuotdi (h',c') tc)) && (kolomattuong (nuocDi m tc));
									   Sy -> (not (vitriQuanLuotdi (h',c') tc)) && (kolomattuong (nuocDi m tc));
									   To -> (not (vitriQuanLuotdi (h',c') tc)) && (kolomattuong (nuocDi m tc));
									   Tj -> (((not (vitriQuanLuotdi (h',c') tc)) && (kolomattuong (nuocDi m tc))) 
									          &&
									          let khongbican (h,c) (h',c') | (tien2 h == Just h') && (phai2 c == Just c') = if vitriTrong (succ h, succ c) tc then True else False
																		   | (lui2 h == Just h') && (phai2 c == Just c') = if vitriTrong (pred h, succ c) tc then True else False
																		   | (tien2 h == Just h') && (trai2 c == Just c') = if vitriTrong (succ h, pred c) tc then True else False
																		   | (lui2 h == Just h') && (trai2 c == Just c') = if vitriTrong (pred h, pred c) tc then True else False
																		   | otherwise = False
												in khongbican (h,c) (h',c')
											 );
									   Ma -> (((not (vitriQuanLuotdi (h',c') tc)) && (kolomattuong (nuocDi m tc))) 
									          &&
									          let khongbican (h,c) (h',c')   | (tien2 h == Just h') && (phai1 c == Just c') = if vitriTrong (succ h, c) tc then True else False
																			 | (tien2 h == Just h') && (trai1 c == Just c') = if vitriTrong (succ h, c) tc then True else False
																			 | (lui2 h == Just h') && (phai1 c == Just c') = if vitriTrong (pred h, c) tc then True else False
																			 | (lui2 h == Just h') && (trai1 c == Just c') = if vitriTrong (pred h, c) tc then True else False
																			 | (tien1 h == Just h') && (phai2 c == Just c') = if vitriTrong (h, succ c) tc then True else False
																			 | (tien1 h == Just h') && (trai2 c == Just c') = if vitriTrong (h, pred c) tc then True else False
																			 | (lui1 h == Just h') && (phai2 c == Just c') = if vitriTrong (h, succ c) tc then True else False
																			 | (lui1 h == Just h') && (trai2 c == Just c') = if vitriTrong (h, pred c) tc then True else False
																			 | otherwise = False
												in khongbican (h,c) (h',c')
											 );
									   Xe -> (((not (vitriQuanLuotdi (h',c') tc)) && (kolomattuong (nuocDi m tc))) 
									          &&
									          let dsk = dskhoang (h,c) (h',c') ; sq = soquantrongdskhoang dsk tc  
												in if sq == 0 then True else False
											 );
									   Ph -> ((vitriQuanLuotdi' (h',c') tc) 
									          &&
									          let dsk = dskhoang (h,c) (h',c') ; sq = soquantrongdskhoang dsk tc  
												in if sq == 1 then True else False
											 ) || 
											 ((vitriTrong (h',c') tc) 
									          &&
									          let dsk = dskhoang (h,c) (h',c') ; sq = soquantrongdskhoang dsk tc  
												in if sq == 0 then True else False
											 ) && (not (vitriQuanLuotdi (h',c') tc)) && (kolomattuong (nuocDi m tc))
											 ;		 
									   }
	| otherwise = False
-- loc ra tat ca ca nuoc di hop le tu 1 the co (thoa nam ca luatVitri, LuatMove, va LuatNuocdi)
dsNuocdihople :: Theco -> [Move]
dsNuocdihople tc = [x | x <- (concat (map dsVitricothedi (dsQutd tc))), luatNuocdi x tc]
-- nhan vao 1 the co tra lai 1 ds cac the co hop le
dstheco :: Theco -> [Theco]
dstheco tc =[nuocDi x tc | x <- dsNuocdihople tc]
--luong gia 1 tc bang tong so the co hop le sinh ra tu the co hien tai
luongGia :: Theco -> Int
luongGia tc = length (dsNuocdihople tc)
-- sap xep cac the co sinh ra theo muc luong gia tang dan
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted
maxtc :: [Theco] -> Theco
maxtc [] = error "maxtc of empty list"
maxtc [tc] = tc
maxtc (tc:tcs) 
    | luongGia tc > luongGia maxTail = tc
    | otherwise = maxTail
    where maxTail = maxtc tcs
mintc :: [Theco] -> Theco
mintc [] = error "mintc of empty list"
mintc [tc] = tc
mintc (tc:tcs) 
    | luongGia tc <= luongGia minTail = tc
    | otherwise = minTail
    where minTail = mintc tcs

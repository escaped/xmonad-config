module Tools

where

--Search engines to be selected :  
-- [google (g), wikipedia (w) , youtube (y) , maps (m), dictionary (d) , wikipedia (w), bbs (b) ,aur (r), wiki (a) ]
--keybinding: hit mod + s + <searchengine>
searchEngineMap method = M.fromList $
	[ ((0, xK_g), method S.google )
	, ((0, xK_y), method S.youtube )
	, ((0, xK_m), method S.maps )
	, ((0, xK_d), method S.dictionary )
	, ((0, xK_w), method S.wikipedia )
	, ((0, xK_b), method $ S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
	, ((0, xK_r), method $ S.searchEngine "AUR" "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=")
	, ((0, xK_a), method $ S.searchEngine "archwiki" "http://wikiarchlinux.org/index.php/Special:Search?search=")
	]


\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Castles in the Air"}}
  composer = \markup\oldStyleNum\italic"Bonnie Jean o’ Aberdeen"
  poet = \markup\oldStyleNum"James Ballantine (1808–1877)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 80))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  \slurDashed
}

sopMusic = \relative c' {
	\partial 8 ees8 |
  ees8.[ ees16]
  ees16 g8. bes,4 bes8[ c] |
  ees8.[ d16] ees8 f g4. \teeny g8 \normalsize |
  aes8.[ g16] aes8 c bes8.[ g16] f8[ ees] |

  f[ g] f[ ees] c4. \teeny ees8 \normalsize |
  ees8.[ ees16] ees8 g bes,16[ bes8.] bes8.[ c16] |
  ees8.[ ees16] ees8. f16 g4. \teeny g8 \normalsize |

  aes8.[ bes16] c8[ ees] bes16[ g8.] ees8 f |
  g8 aes g8. f16 ees4. \bar"||" g16[ bes] |
  c8.[ c16] c8 ees bes4~ bes8 g |

  aes8.[ g16] aes8 bes g4. g8 |
  aes8.[ aes16] aes8 c bes8( g4.*2/3) \teeny ees8 \normalsize |
  f8. g16 f8 ees c4 g'8.[ f16] |

  ees8. ees16 ees g8. bes,4 bes8.[ c16] |
  ees8 d ees8. f16 g4. \teeny g16 g \normalsize |
  aes8.[ bes16] c8 ees bes4*1/2 \teeny g8 \normalsize ees8 f |

  g8 aes g8. f16 ees4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	The bon -- nie, bon -- nie bairn sits _ pok -- in’ in the ase,
  "" Glow’ -- rin’ in the fire _ wi’ his wee _ round _ face;
  "" Laugh -- in’ at the fuf -- fin’ lowe— _ what _ sees he there?
  
  "" Ha! the young _ dream -- er’s big -- gin’ cas -- tles in the air!
  His _ wee _ chub -- by face, an’ his tow -- zy cur -- ly pow
  Are laugh -- _ in’ an’ nod -- din’ "" to the danc -- in’ lowe;
  He’ll _ brown his ros -- y cheeks and _ singe his sun -- ny hair,
  "" "" Glow -- rin’ at the imps "" wi’ their cas -- tles in the air!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  He sees _ muck -- le cas -- tles _ tow -- ’rin’ to the moon,
  He sees _ lit -- tle sodg -- _ ers _ pu’ -- in’ them a’ doon;
  "" Warlds _ whom -- lin’ up and doun, _ blaz -- in’ wi’ a flare,
  
  "" Losh! _ how he loups _ as they glim -- mer in the air!
  For _ a’ sae sage he looks, _ what can the lad -- die ken?
  He’s think -- in’ up -- on nae -- thing, like mon -- y migh -- ty men;
  A _ wee thing mak’s us think, a _ sma’ thing mak’s us stare,—
  There are mair _ folk than him "" big -- gin’ cas -- tles in the air!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  "" Sic a night in win -- ter may weel _ mak’ him cauld;
  His chin up -- on his buf -- fy hand will soon _ mak’ him auld;
  His brow is bent sae braid, _ oh _ pray that Dad -- dy Care
  
  Wad let the wean a -- lane _ wi’ his cas -- tles in the air.
  He’ll _ glow -- er at the fire, an’ he’ll keek _ at the light;
  But mon -- y spark -- ling stars _ are swal -- lowed up by night;
  "" _ Auld -- er een than his are _ glam -- our’d by a glare,
  "" "" Hearts are bro -- ken, heads are turn’d wi’ cas -- tles in the air!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  bes8 |
  bes8.[ bes16] bes ees8. bes4 bes8[ aes] |
  bes8.[ bes16] bes8 d ees4. \teeny ees8 \normalsize |
  ees8.[ ees16] ees8 ees ees8.[ ees16] bes8 ees |

  c[ ees] c[ c] aes4. \teeny bes8 \normalsize |
  bes8.[ bes16] bes8 ees bes16[ bes8.] bes[ aes16] |
  bes8.[ bes16] bes8. d16 ees4. \teeny ees8 \normalsize |

  c8.[ ees16] ees8[ aes] g16[ ees8.] c8 ees |
  ees8 ees ees8. d16 ees4. \bar"||"
  ees8 |
  ees8.[ ees16] ees8 aes g4~ g8 ees |

  d8.[ d16] d8 f ees4. ees8 |
  c8.[ c16] c8 ees g( ees4.*2/3) \teeny ees8 \normalsize |
  c8. ees16 c8 c aes4 d |

  ees8. ees16 ees ees8. bes4 bes8.[ aes16] |
  bes8 bes bes8. bes16 ees4. \teeny ees16 ees \normalsize |

  c8.[ ees16] ees8 aes g4*1/2 \teeny ees8 \normalsize c8 ees |
  ees8 ees ees8. d16 ees4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  g8 |
  g8.[ g16] g bes8. g4 g8[ aes] |
  g8.[ bes16] g8 aes bes4. \teeny bes8 \normalsize |
  c8.[ bes16] c8 aes g8.[ bes16] aes8[ g] |

  aes[ bes] aes[ aes] ees4. \teeny g8 \normalsize |
  g8.[ g16] g8 bes g16[ g8.] g[ aes16] |
  g8.[ g16] g8. aes16 bes4. \teeny bes8 \normalsize |

  aes8.[ g16] aes8[ aes] bes16[ bes8.] a8 a |
  bes8 bes bes8. aes!16 g4. \bar"||"
  bes16[ g] |
  aes8.[ aes16] aes8 aes bes4~ bes8 bes |

  bes8.[ bes16] bes8 aes bes4. bes8 |
  aes8.[ aes16] aes8 aes bes8~ bes4.*2/3 \teeny g8 \normalsize |
  aes8. bes16 aes8 aes ees4 aes4 |

  g8. g16 g bes8. g4 g8.[ aes16] |
  g8 bes g8. aes16 bes4. \teeny bes16 bes \normalsize |
  aes8.[ g16] aes8 aes bes4*1/2 \teeny bes8 \normalsize a8 a |
  bes8 bes bes8. aes!16 g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees8.[ ees16] ees ees8. ees4 ees8[ ees] |
  ees8.[ f16] ees8 bes ees4. \teeny ees8 \normalsize |
  aes,8.[ bes16] aes8 c ees8.[ ees16] d8 ees |

  aes,8[ g] aes[ aes] aes4. \teeny ees'8 \normalsize |
  ees8.[ ees16] ees8 ees ees16[ ees8.] ees8.[ ees16] |
  ees8.[ ees16] ees8. bes16 ees4. \teeny ees8 \normalsize |

  aes,8.[ ees'16] aes,8[ c] ees16[ ees8.] c8 c |
  bes8 bes bes8. bes16 ees4.
  ees8 |
  aes,8.[ aes16] aes8 c ees4~ ees8 ees |
  
  bes8.[ bes16] c8 d ees4. ees8 |
  aes,8.[ aes16] aes8 c ees8~ ees4.*2/3 \teeny ees8 \normalsize|
  aes,8. g16 aes8 aes aes4 bes |

  ees8. ees16 ees ees8. ees4 ees4 |
  ees8 f ees8. d16 ees4. \teeny ees16 ees \normalsize |
  aes,8.[ ees'16] aes,8 c ees4*1/2 \teeny ees8 \normalsize c8 c |

  bes8 bes bes8. bes16 ees4
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 75
    \set Staff.midiInstrument = "flute"
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
}

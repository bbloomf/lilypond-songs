\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #48
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8 g'8 |
  g4 g8 d'4 b8 |
  a4 g8 fis4 a8 |
  
  b4 e,8 e4 fis8 |
  e4 d8 d4 d8 |
  e4 e8 g4 g8 |
  a4 b8 d4 d8 |

  e4 e8 d4 b8 |
  a4 g8 g4. \bar"||"\break
  g4 g8 d'4 b8 |

  a4 g8 fis4. |
  b4 e,8 e4 fis8 |
  e4 d8 d4. |
  e4 e8 g4 g8 |

  a4 b8 d4 d8 |
  e4 e8 d4 b8 |
  a4 g8 g4. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The wind is fair, the day is fine,
  And swift -- ly, swift -- ly runs the time;
  The boat is float -- ing on the tide
  That wafts me off from Fiu -- na -- ry.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  A thou -- sand, thou -- sand ten -- der ties
  A -- wake this day my plain -- tive sighs,
  My heart with -- in me al -- most dies
  At thought of leav -- ing Fiu -- na -- ry.

  We must up and haste a -- way,
  We must up and haste a -- way,
  We must up and haste a -- way,
  Fare -- well, fare -- well to Fiu -- na -- ry.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  But I must leave those hap -- py vales, ""
  See, they spread the flap -- ping sails!
  A -- dieu, a -- dieu my na -- tive dales!
  Fare -- well, fare -- well to Fiu -- na -- ry.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'8 |
  d4 d8 g4 d8 |
  e4 e8 d4 fis8 |
  g4 e8 e4 d8 |
  c4 d8 d4 d8 |

  b4 b8 e4 e8 |
  e4 e8 fis4 fis8 |
  g4 g8 g4 g8 |
  e[ c] e d4. \bar"||"

  b8[ d] g fis4 fis8 |
  e[ c] e d4.
  g8[ e] e c4 d8 |
  c[ a] d d4. |

  e4 e8 e4 e8 |
  e[ c] e fis4 fis8 |
  g[ e] g g4 g8 |
  e[ c] c b4.
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
  b4 b8 g4 g8 |
  c4 c8 a4 a8 |
  b4 g8 g4 a8 |
  g4 fis8 fis4 fis8 |

  g4 g8 g4 g8 |
  c4 b8 b4 b8 |
  c[ e] c b[ g] b |
  c4 c8 b4. \bar"||"

  g4 b8 d[ b] d |
  c4 c8 a[ fis a] |
  b4 g8 g[ a] a |
  a4 fis8 fis[ d fis] |

  g[ b] g g[ e] g |
  c4 b8 b[ d] b |
  c4 c8 b[ d] d |
  c4 g8 g4. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g8 |
  g,4 g8 b4 b8 |
  c4 c8 d4 d8 |

  e4 e8 e4 d8 |
  c4 d8 d4 d8 |
  e4 e8 c4 c8 |
  a4 g8 b4 b8 |
  
  c4 c8 g4 g8 |
  c4 e8 g4. \bar"||"
  g,4 g8 b4 b8 |

  c4 c8 d4. |
  e4 e8 c4 d8 |
  a4 d8 d4. |
  e4 e8 c4 c8 |

  a4 g8 b4 b8 |
  c4 c8 g4 b8 |
  c4 d8 g,4. \bar"|."
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
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Farewell to Fiunary"}}
  composer = \markup\oldStyleNum"Norman MacLeod (1812–1872)"
  tagline = ""
}}
global = {
  \key a \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  e8.^\markup\italic"Chorus" fis16 e8 e cis e |
  a a b cis4. |
  d8. cis16 b8 cis8. b16 a8 |
  fis fis e e4. |

  e8. fis16 e8 e cis e |
  a a b cis4. |
  e8. cis16 a8 cis4 b8 |
  a4. a \bar"||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark\markup\italic"Fine"

  e'8 cis a a b cis |
  b8. a16 b8 e,4. |
  e8 a a a b cis |
  d8. cis16 d8 b4. |
  e8 cis a a b cis |
  b8. a16 b8 e,4 \bar"" d'8 |

  cis8. b16 a8 cis cis b |
  a4. a4. \bar"||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark"D.C."
}
sopWords = \lyricmode {
  West -- er -- ing home, and a song in the air,
  Light in the eye and it’s good -- bye to care.
  Laugh -- ter o’ love, and a wel -- com -- ing there,
  Isle of my heart, my own one.
  \set stanza = #"1. "
  Tell me o’ lands o’ the O -- ri -- ent gay;
  Speak o’ the rich -- es and joys o’ Ca -- thay.
  Eh, but it’s grand to be wak -- in’ ilk day
  To find your -- self near -- er to Is -- la.
}

sopWordsII = \lyricmode {
  \repeat unfold 37 ""
  \set stanza = #"2. "
  Where are the folk like the folk o’ the west?
  Can -- ty and cou -- thy and kind -- ly, the best;
  There I would hie me and there I would rest
  At hame wi’ my ain folk in Is -- la.
}

sopWordsIII = \lyricmode {
  %{\repeat unfold 37 ""
  \set stanza = #"3. "
  Now I’m at home and at home I do lay
  Dream -- ing of rich -- es that come from Ca -- thay;
  I’ll hop a good ship and be on my way,
  And bring back my for -- tune to Is -- la.%}
}

sopWordsIV = \lyricmode {
}

sopWordsV = \lyricmode {
}

altoMusic = \relative c' {
  cis8. cis16 cis8 cis a cis |
  e8 e gis a4. |
  fis8. fis16 gis8 a8. gis16 e8 |
  d d d cis4. |

  cis8. cis16 cis8 cis a cis |
  e8 e gis a4. |
  a8. e16 fis8 e4 d8 |
  cis4. cis \bar"||"


  cis8 e cis cis fis fis |
  fis8. fis16 fis8 d4. |
  cis8 cis cis cis fis fis |
  fis8. fis16 fis8 gis4. |

  cis,8 e cis cis fis fis |
  fis8. fis16 fis8 d4 fis8 |
  e8. d16 fis8 e e d |
  cis4. cis \bar"||"
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
  e,8. a16 e8 e e e |
  a a e e4. |
  a8. a16 b8 e,8. e16 a8 |
  a a a a4. |

  e8. a16 e8 e e e |
  a a e e4. |
  a8. a16 cis8 a4 gis8 |
  a4. a \bar"||"


  a8 a e fis a a |
  d8. d16 d8 b4. |
  a8 e e fis a a |
  b8. a16 b8 d4. |

  a8 a e fis a a |
  d8. d16 d8 b4 a8 |
  a8. gis16 cis8 a a gis |
  a4. a \bar"||"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a,8. a16 a8 a a a |
  cis cis e a,4. |
  d8. fis16 e8 a,8. a16 cis8 |
  d d d a4. |

  a8. a16 a8 a a a |
  cis cis e a,4. |
  cis8. cis16 fis8 e4 e8 
  a,4. a \bar"||"

  a8 a a fis fis fis |
  b8. d16 b8 a4. |
  a8 a a fis fis fis |
  b8. cis16 b8 e4. |

  a,8 a a fis fis fis |
  b8. d16 b8 a4 d8 |
  a8. e'16 fis8 e e e 
  a,4. a \bar"||"
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
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Westering Home"}}
  composer = \markup\oldStyleNum"Hugh S. Roberton"
  tagline = ""
}}
global = {
  \key c \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  a'8 a g a a g |
  a a g e4 \bar"" c'16[ d] |
  e8 e d16[ c] c8 a c16[ a] |
  g8 e g a4 \bar"" c16[ d] |

  e8 e d16[ c] d8 d e16[ d] |
  c8 a g16[ e] d4 \bar"" c'16[ a] |
  g8 e c d c d |
  e c' g a4. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Three score o’ no -- bles rade up the King’s ha’
  But bon -- nie Glen -- lo -- gie’s the flow’r o’ them a’
  Wi’ his milk -- white steed and his bon -- nie black e’e,
  “Glen -- lo -- gie, dear mo -- ther, Glen -- lo -- gie for me!”
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  “Haund your tongue, doch -- ter, there’s bet -- ter than he,”
  “O say na sae, mo -- ther, for that can -- na be;
  Tho’ Doum -- lie is great -- er and rich -- er than he,
  Yet if I maun tak’ him, I’ll cer -- tain -- ly dee.”
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  “There is, Glen -- lo -- gie, a let -- ter for thee,
  O there is, Glen -- lo -- gie, a let -- ter for thee!”
  The first line he look’d at, a licht lauch lauched he,
  But ere he had read thro’t tears blind -- ed his e’e.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Then to Glen -- fel -- dy’s but sma’ mirth was there,
  An bon -- nie Jean’s mo -- ther was tear -- in’ her hair,
  “Ye’re wel -- come, Glen -- lo -- gie, ye’re wel -- come,” quo’ she,
  “Ye’re wel -- come, Glen -- lo -- gie, your Jea -- nie to see.”
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Pale and wan was she when_Glen -- lo -- gie gae’d ben,
  But ro -- sy red grew she when e’er he sat down;
  She turned _ a -- wa’ wi’ a smile in her e’e.
  “O din -- na fear, mo -- ther, I’ll may -- be no dee!”
}

altoMusic = \relative c' {
  e8 e e e e e |
  e e e c4 e16[ g] |
  g8 g g16[ e] e8 e f |
  e c e e4 e16[ g] |

  g8 g g16[ e] g8 g c,16[ g'] |
  e8 f e16[ c] a4 c8 |
  c c c a a a |
  c c b c4. \bar"|."
}
tenorMusic = \relative c' {
  c8 c b c c b |
  c c b g4 a16[ d] |
  c8 c g16[ a] a8 c a |
  g g g c4 a16[ d] |
  
  c8 c g16[ a] b8 b g |
  a c c f,4 a8 |
  g g g f a f |
  a a g e4. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a8 a e a a e |
  a, a b c4 a16[ b] |
  c8 c b16[ a] a8 a f |
  c' c c a4 a16[ b] |
  
  c8 c b16[ a] g8 g c16[ b] |
  a8 f c' d4 f8 |
  e c e d e d |
  e e e a,4. \bar"|."
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
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Glenlogie"}}
  composer = \markup\oldStyleNum"Scottish Folk Song"
  tagline = ""
}}

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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #191
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
  \key a \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4
  e4 |
  a a b |
  cis2 a4 |
  d d cis |
  b2 \bar""
  e,4 |
  a a b |

  cis2 d4 |
  e8[ d] cis4 b |
  a2 \bar"||"
  e4 |
  e e fis |
  gis gis a |

  b b cis |
  d2 \bar""
  e,4 |
  a a b |
  cis cis d |
  e8[ d] cis4 b |
  a2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  O Wor -- ship the King all glo -- rious a -- bove,
  O grate -- ful -- ly sing His pow’r and His love;
  Our Shield and De -- fen -- der, 
  The An -- cient of days,
  Pa -- vil -- ioned in splen -- dor, 
  And gird -- ed with praise.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "

  O tell of His might, O sing of His grace,
  Whose robe is the light, 
  Whose can -- o -- py space.
  His cha -- riots of wrath
  The deep thun -- der -- clouds form,
  And dark is His path 
  On the wings of the storm.

}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "

  The earth with its store of won -- ders un -- told,
  Al -- might -- y, Thy pow’r 
  Hath found -- ed of old;
  Hath stab -- lished it fast 
  By a change -- less de -- cree,
  And round it hath cast, 
  Like a man -- tle, the sea.

}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "

  Thy boun -- ti -- ful care 
  What tongue can re -- cite?
  It breathes in the air; 
  It shines in the light;
  It streams from the hills; 
  It de -- scends to the plain;
  And sweet -- ly di -- stils in the dew and the rain.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "

  Frail chil -- dren of dust, 
  And fee -- ble as frail,
  In Thee do we trust, 
  Nor find Thee to fail;
  Thy mer -- cies how ten -- der, 
  How firm to the end,
  Our Mak -- er, De -- fen -- der, Re -- deem -- er, and Friend!
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "

  O mea -- sure -- less might! In -- ef -- fa -- ble love!
  While an -- gels de -- light 
  To hymn Thee a -- bove,
  The hum -- bler cre -- a -- tion, 
  Though fee -- ble their lays,
  With true a -- do -- ra -- tion 
  Shall sing to Thy praise.
}

altoMusic = \relative c' {
  cis4 |
  cis e e |
  e2 e4 |
  fis e e |
  e2
  e4 |
  cis e e |
  
  e2 fis4 |
  e8[ fis] e4 d |
  cis2 \bar"||"
  e4 |
  e e dis |
  d! d cis |

  b e e |
  <e gis>2
  e4 |
  e e e |
  e e fis |
  e8[ fis] e4 d |
  cis2 \bar"|."
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
  a4 |
  a a gis |
  a2 a4 |
  a b a |
  gis2
  e4 |
  e a gis |

  a2 a4 |
  a8[ a] a4 gis |
  a2
  gis4 |
  gis gis a |
  b b a |

  gis gis a |
  b2 e,4 |
  cis' a gis |
  a a a |
  a a gis |
  a2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a,4 |
  a cis e |
  a2 cis,4 |
  fis gis a |
  e2
  e4 |
  a,4 cis e |
  
  a2 fis4 |
  cis8[ d] e4 e |
  a,2
  e'4 |
  e e e |
  e e e |

  e e e |
  e2
  e4 |
  a, cis e |
  a a fis |
  cis8[ d] e4 e |
  a,2 \bar"|."
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
    %\new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    %\new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
    \new Lyrics = "altosVI"  \lyricsto "sopranos" \sopWordsVI
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"O Worship the King All Glorious Above"}}
  poet = \markup\oldStyleNum"Robert Grant (1785–1838)"
  composer = \markup\oldStyleNum"Franz Josef Haydn (1732–1809)"
  tagline = ""
}}

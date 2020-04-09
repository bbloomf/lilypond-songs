\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"To Phœbe"}}
  poet = \markup\oldStyleNum"W. S. Gilbert (1836–1911)"
  composer = \markup\oldStyleNum"John Frederick Bridge (1844-1924)"
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
  ragged-last-bottom = ##f
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
  \key e \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \repeat unfold 2 {
  	gis'4. e8 |
    fis gis fis gis |
    a4 gis-. |
    gis4.  fis8 |
    fis gis fis cis |
    dis4 b'\rest |

    gis4. fis8 |
    e a gis fis |
    \slurDashed fisis4( gis-.) |
    \slurSolid
    fis4. gis8 |
    b16[ a] cis,[ fis] e8-. dis-. |
    e2 |
  }
  
  %page2
  e4. c8 |
  c d e f |
  f4 e-. |
  d4. cis!8 |
  d cis d dis |
  e2 |
  gis!4. e8 |

  %page3
  e8 fis gis a |
  a4-. gis-. |
  b\rest fis8. c16 |
  b4 e |
  e4. fis8 |
  gis2 |
  a4. fis8 |

  e4 b-. |
  b'4\rest\fermata fis8. gis16 |
  e2\fermata \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  “Gen -- tle, mod -- est, lit -- tle flow -- er,
  Sweet e -- pi -- to -- me of May,
  Love me but for half an hour,
  Love me, love me lit -- tle fay,

  Gen -- tle, mod -- est, lit -- tle flow -- er,
  Sweet e -- pi -- to -- me of May,
  Love me but for half an hour,
  Love me, love me lit -- tle fay.”

  Sen -- ten -- ces so fierce -- ly flam -- ing
  In your ti -- ny, shell -- like ear;
  I should

  al -- ways be ex -- claim -- ing—
  If I loved you, Phœ -- be dear,
  if I loved you, Phœ -- be dear!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  “Smiles that thrill from a -- ny dis -- tance,
  Shed up -- on me while I sing!
  Please ec -- sta -- ti -- cise \set ignoreMelismata = ##t ex -- ist -- ence,
  \unset ignoreMelismata
  Love me, oh thou fai -- ry thing!
  Smiles that thrill from a -- ny dis -- tance,
  Shed up -- on me while I sing!
  Please ec -- sta -- ti -- cise \set ignoreMelismata = ##t ex -- ist -- ence;
  \unset ignoreMelismata
  Love me, oh thou fai -- ry thing!”


  Words like these out -- pour -- ing sad -- ly,
  You’d per -- pet -- u -- al -- ly hear,
  If I loved you, fond -- ly, mad -- ly—
  But I do not, Phœ -- be dear!
  but I do not, Phœ -- be dear!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    e4. b8 |
    dis e dis e |
    fis4 e-. |
    e4. e8 |
    e e e ais, |
    b4 s |

    bis4. bis8 |
    cis e dis dis |
    \tieDashed dis4~ dis |
    \tieSolid
    cis4. cis8 |
    cis cis b-. b-. |
    b2 |
  }

  %page2
  c4. c8 |
  c c c c |
  b4 c-. |
  b4. b8 |
  b b b b |
  c2 |
  b4. b8 |

  %page3
  b b b e |
  dis4-. e-. |
  s c8. c16 |
  b4 b |
  cis!4. cis8 |
  dis2 |
  fis4. cis8 |

  b4 gis-. |
  s dis'8. dis16 |
  e2 |
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
  \repeat unfold 2 {
    b4. gis8 |
    c b c b |
    dis4 e-. |
    ais,4. ais8 |
    ais b ais fis |
    fis4 s |

    gis4. gis8 |
    gis cis b cis |
    \slurDashed cis4( bis-.) |
    \slurSolid
    cis4. b8 |
    a a gis-. fis-. |
    gis2 |
  }

  %page2
  g4. e8 |
  e f g aes |
  aes4 g-. |
  aes4. aes8 |
  aes aes aes aes |
  g2 |
  gis!4. gis8 |

  %page3
  gis a b c |
  c4-. b-. |
  s a8. a16 |
  gis4 gis |
  a4. cis8 |
  bis2 |
  cis4. a8 |

  gis4 e-. |
  s a8. b16 |
  gis2
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {

  \repeat unfold 2 {
    e,4. e8 |
    e e e e |
    e4 e-. |
    fis4. fis8 |
    fis fis fis, fis |
    b4 d\rest |

    gis,4. gis8 |
    cis a b a |
    \slurDashed a4( gis-.) |
    \slurSolid
    a4. gis8 |
    fis a b-. b-. |
    e2 |
  }
  %page2
  c4. c8 |
  c c c c |
  c4 c-. |
  f4. f8 |
  f f f f |
  c2 |
  e4. e8 |

  %page3
  e e e e |
  e4-. e-. |
  d\rest a8. a16 |
  b4 b |
  a4. a'8 |
  gis2 |
  fis4. a,8 |

  b4 b-. |
  d\rest\fermata b8. b16 |
  e,2\fermata
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
    \tempo 4 = 90
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

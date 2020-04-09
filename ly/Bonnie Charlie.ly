\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Bonnie Charlie"}}
  composer = \markup\oldStyleNum"Lady Nairne"
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 8 \teeny c8 | \normalsize
  f4. g8 a4 f |
  d f c2*3/4 \teeny c'8 | \normalsize
  c4. d8 c4 a |
  \slurDashed a( g) g2*3/4 \teeny c,8 | \normalsize
  
  \slurSolid
  f4. g8 a4 f |
  d f c2 |
  c'4. d8 c4 a |
  \slurDashed g4.( f8) f2\fermata \bar"||"

  \slurSolid
  a8 c4. c4 c |
  a4. c8 c2 |
  f4. e8 d4 c |
  d8( c4) a8 g2\fermata |

  a8 a4. g4 f |
  d8 f4. c2 |
  d'4. d8 c4 a |
  a8( g4) f8 f2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  "" Bon -- nie Char -- lie’s now a -- wa;
  "" Safe -- ly owre the friend -- ly main;
  "" Mon -- y~a heart will break in twa,
  Should he ne’er come back a -- gain.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Ye trust -- ed in your Hie -- land men,
  They trust -- ed you, dear Char -- lie!
  They kent your hid -- ing in the glen,
  Death or ex -- ile brav -- ing.

  Will ye no come back a -- gain?
  Will ye no come back a -- gain?
  Bet -- ter lo’ed ye can -- na be—
  Will ye no come back a -- gain?
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  "" Eng -- lish bribes were a’ in vain,
  Tho’ puir and puir -- er we maun be;
  "" Sil -- ler can -- na buy the heart
  That beats aye for thine and thee.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 8 \teeny c8 | \normalsize
  c4. c8 f4 c |
  bes d a2*3/4 \teeny f'8 | \normalsize
  f4. f8 f4 f |
  \slurDashed f( e) e2*3/4 \teeny c8 | \normalsize

  \slurSolid
  c4. c8 f4 c |
  bes d a2 |
  f'4. f8 f4 f |
  \slurDashed e4.( c8) c2 |

  \slurSolid
  f8 f4. e4 g |
  f4. e8 e2 |
  f4. f8 f4 g |
  f8~ f4 f8 e2 |

  f8 f4. e4 d |
  bes8 d4. a2 |
  f'4. f8 f4 f |
  f8( e4) c8 c2 \bar"|."
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
  \partial 8 \teeny a8 | \normalsize
  a4. bes8 c4 a |
  f f f2*3/4 \teeny a8 | \normalsize
  a4. bes8 a4 c |
  \tieDashed c~ c c2*3/4 \teeny a8 | \normalsize
  
  \tieSolid
  a4. bes8 c4 a |
  f f f2 |
  a4. bes8 a4 c |
  \tieDashed bes4.~ bes8 a2 |

  \tieSolid
  c8 a4. bes4 bes |
  c4. bes8 bes2 |
  a4. c8 bes4 bes |
  bes8( a4) c8 c2 |

  c8 c4. bes4 a |
  f8 bes4. f2 |
  bes4. bes8 a4 c |
  c8( bes4) a8 a2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 8 \teeny f,8 | \normalsize
  f4. f8 f4 f |
  bes, bes f2*3/4 \teeny f'8 | \normalsize
  f4. f8 f4 f |
  \tieDashed c~ c c2*3/4 \teeny f8 | \normalsize

  \tieSolid
  f4. f8 f4 f |
  bes, bes f2 |
  f'4. f8 f4 f |
  \tieDashed c4.~ c8 f,2\fermata |

  \tieSolid
  f'8 f4. g4 e |
  f4. c8 c2 |
  f4. f8 f4 e |
  f8~ f4 c8 c2\fermata |

  f8 f4. c4 d |
  bes8 bes4. f2 |
  bes4. bes8 f'4 f |
  c8~ c4 c8 f,2 \bar"|."
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
    \tempo 4 = 105
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

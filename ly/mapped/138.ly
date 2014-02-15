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
  first-page-number = #138
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
  \key c \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8 g'8 |
  g8. c16 c8 b16[ c] f8 d |
  c4( b) c |
  a4. g8 \times2/3{f[ e] d} |
  c8 c'4 b8\rest \bar""\break g4 |
  g8. c16 c8 b16[ c] f8 d |
  c4( b) c |
  
  a4. g8 \times2/3{f[ e d]} |
  c8 c'4 b8\rest \bar""\break g4 |
  g8 e g4 bes |
  a8 f a4. e'8 |
  
  f4. e8 d c |
  b d b\rest \bar""\break g f e |
  d4 c f'8 d |

  c4( b) c8[ b] |
  a4. g8 \times2/3{f[ e d]} |
  c c'4. b8\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Of all the girls that are so smart,
  There’s none like pret -- ty Sal -- ly;
  She is the dar -- ling of my heart,

  And lives in our __ al -- ley:
  There is no la -- dy in the land
  That’s half so sweet as Sal -- ly;
  She is the dar -- ling of my heart,
  And lives in our __ al -- ley.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Of all the days with -- in the week __
  I dear -- ly love but one day;
  And that’s the day that comes be -- tween
  \set ignoreMelismata = ##t
  The Sat -- ur -- day _ and Mon -- day:
  Oh, then I’m dress’d all in my best,
  To walk a -- broad with Sal -- ly;
  \unset ignoreMelismata
  She is the dar -- ling of my heart,
  And lives in our __ al -- ley.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  My mas -- ter and the neigh -- bours all __
  Make game of me __ and Sal -- ly;
  And but for her I’d ra -- ther be __
  \set ignoreMelismata = ##t
  A slave, and row _ a gal -- ley.
  But when my sev’n long years are out,
  Oh, then I’ll mar -- ry Sal -- ly;
  \unset ignoreMelismata
  And then how hap -- pi -- ly we’ll live __
  But not in our __ al -- ley.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e8 |
  e8. e16 c8 d16[ e] d8 f |
  e4( d) g |
  f4. e8 \times2/3{d[ c] b} |
  c8 e4 s8 e4 |

  e8. e16 c8 d16[ e] d8 f |
  e4( d) g |
  f4. e8 \times2/3{d[ c b]} |
  c8 e4 s8 e4 |

  e8 c e4 e |
  f8 c f4. a8 |
  a4. a8 a a |
  g b s g f e |

  d4 c a'8 f |
  e4( d) g |
  f4. e8 \times2/3{d8[ c b]} |
  c8 e4. s8 \bar"|."
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
  c8 |
  c8. g16 g8 g a b |
  c4( b) c |
  c4. c8 \times2/3{b8[ g] f} |
  e g4 s8 c4 |

  c8. g16 g8 g a b |
  c4( b) c |
  c4. c8 \times2/3{b8[ g f]} |
  e g4 s8 c4 |

  c8 g c4 g |
  c8 a c4. c8 |
  d4. c8 d d |
  d8 b8 s g f e |

  d4 c d8 a' |
  c4( b) c |
  c4. c8 \times2/3{b[ g f]} |
  e8 g4. s8 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,8 |
  c8. c16 e8 d16[ c] d8 d |
  g4( g8[) f] e[ c] |
  f4. g8 \times2/3{g,4 g8} |

  c8 c4 d8\rest c4 |
  c8. c16 e8 d16[ c] d8 d |
  g4( g8[) f] e[ c] |
  
  \tieDashed f4. g8 \times2/3{g,4~ g8} |
  \tieDotted
  c8 c4 d8\rest c4 |
  c8 c c4 c |
  f8 f f4. e8 |

  d4. e8 f fis |
  g8 g, d'\rest g f e |
  d4 c d8 f |

  g4( g,) e'4 |
  f4. g8 g,4 |
  c8 c4. d8\rest \bar"|."

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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Sally in our Alley"}}
  poet = \markup\oldStyleNum"Henry Carey (1687–1743)"
  composer = \markup\oldStyleNum"17th or 18th Century English Folk Song"
  tagline = ""
}}

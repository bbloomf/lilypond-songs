\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Uist Tramping Song"}}
  composer = \markup\oldStyleNum"John R. Bannerman"
  poet = \markup\oldStyleNum"Hugh S. Roberton"
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
       (padding . -3)
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
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 d8^\markup\italic"Chorus" e |
  g4 d8 e g4 \bar"" g8 b |

  d8. d16 d8 b d d \bar"" b a |
  g4 g8 b d4 \bar"" b8 d |
  e8. e16 d8 b b a \bar"" b d |
  e8. e16 e8 d b a \bar"" g a |

  b8. a16 g8 e e d \bar"" d e |
  g8. g16 g8 g e' d \bar"" b a |
  g4*1/4 d8 e g4*7/4\fermata \bar"||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark\markup\italic"Fine"

  d8[ e] |
  g d b'8. a16 g4 \bar"" g8 b |
  d[ d] d8. b16 d4 \bar"" b8 a |

  g8. g16 g8 b d[ d] \bar"" b d |
  e8. e16 d8 b a4 \bar"" b8 d |

  e8. e16 e8 d b a \bar"" g a |
  b8. a16 g8 e e[ d] \bar"" d e |
  g8. g16 g8 g e' d \bar"" b a |
  g4 g g \bar"||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark"D.C."
}
sopWords = \lyricmode {
  Come a -- long, come a -- long,
    Let us foot it out to -- geth -- er;
  Come a -- long, come a -- long,
    Be it fair or storm -- y wea -- ther,
  With the hills of home be -- fore us
    And the pur -- ple of the hea -- ther,
  Let us sing in hap -- py cho -- rus,
    Come a -- long, come a -- long.


  \set stanza = #" 1."
  So gai -- ly sings the lark,
    And the sky’s all a -- wake
  With the prom -- ise of the day,
    For the road we glad -- ly take;
  So it’s heel and toe and for -- ward,
    Bid -- ding fare -- well to the town,
  For the wel -- come that a -- waits us
    Ere the sun goes down.
}

sopWordsII = \lyricmode {
  \repeat unfold 58 ""
  \set stanza = #" 2."
  \set ignoreMelismata = ##t
  It’s the call of sea and shore,
    It’s the tang of bog and peat,
  And the scent of brier and myr -- tle
    That puts ma -- gic in our feet;
  So it’s on we go re -- joic -- ing,
    O -- ver brack -- en, o -- ver stile; _
  And it’s soon we will be tramp -- ing
    Out the last long mile.
}

sopWordsIII = \lyricmode {

}

sopWordsIV = \lyricmode {
}

sopWordsV = \lyricmode {
}

altoMusic = \relative c' {
  d8 c |
  b4 b8 c d4 d8 d |
  fis8. fis16 fis8 fis fis fis fis fis |
  g4 e8 g fis4 fis8 fis |
  g8. g16 g8 g g fis fis fis |

  g8. g16 g8 fis g d e fis |
  fis8. fis16 e8 c c d d c |
  d8. d16 d8 d g fis fis fis |
  d4*1/4 d8 c b4*7/4 \bar"||"


  b8[ c] |
  b b d8. c16 d4 d8 d |
  fis[ fis] fis8. fis16 fis4 fis8 fis |
  g8. g16 e8 g fis[ fis] fis fis |
  g8. g16 g8 g fis4 fis8 fis |

  g8. g16 g8 fis g d e fis |
  fis8. fis16 e8 c c[ d] d c |
  d8. d16 d8 d g fis fis fis |
  g4 e g \bar"||"
}
altoWords = \lyricmode {
}
tenorMusic = \relative c' {
  fis,8 g |
  d4 g8 g b4 b8 g |
  b8. b16 b8 d b b d c |
  b4 b8 b b4 d8 b |
  c8. c16 b8 d d d d c |

  c8. c16 c8 c d d b c |
  d8. c16 c8 g g fis fis g |
  b8 b b b b b d c |
  b4*1/4 fis8 g g4*7/4 \bar"||"


  g8[ g] |
  d g g8. g16 b4 b8 g |
  b[ b] b8. d16 b4 d8 c |
  b8. b16 b8 b b[ b] d b |
  c8. c16 b8 d d4 d8 c |

  c8. c16 c8 c d d b c |
  d8. c16 c8 g g[ fis] fis g |
  b8 b b b b b d c |
  b4 c b4 \bar"||"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,8 d |
  g,4 g8 c g4 g8 g |
  b8. b16 b8 b b b d d |
  e4 e8 e b4 b8 b |
  c8. c16 g8 g d' d b d |
  
  c8. c16 c8 d g fis e d |
  b8. d16 c8 c d d d d |
  g g fis fis e d d d |
  g,4*1/4 d'8 d g,4*7/4\fermata \bar"||"


  g8[ c] |
  g8 g g8. g16 g4 g8 g |
  b[ b] b8. b16 b4 d8 d |
  e8. e16 e8 e b8[ b] b b |
  c8. c16 g8 g d'4 b8 d |

  c8. c16 c8 d g fis e d |
  b8. d16 c8 c d4 d8 d |
  g8 g fis fis e d d d |
  g,4 c g4 \bar"||"
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
    \tempo 4 = 95
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

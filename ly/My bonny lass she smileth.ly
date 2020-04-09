\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"My bonny lass she smileth"}}
  composer = \markup\oldStyleNum"Thomas Morley (1557–1602)"
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
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 60))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 60))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #10
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
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \repeat volta 2 {
    d'4 d8 d d4 e |
    d2 d |
    d4 d8 d g4 fis |
    e2 d |
    r1 |
    g4. g8 fis4. fis8 |
    e4 e d d |
    d d d cis |
    
    d2. c?4 |
    b2 a~ |
    a4 g g2~ |
    g fis 
  }
  \alternative { { g1 } { g2 r4 d' }}

  \repeat volta 2 {
    e4 d b cis |
    d2 r4 f |
    e d d cis |
    \time 3/4 d2 r4 |
    
    d4. e8 fis4 |
    g2 fis4 |
    e2 d4 |
    d2 cis4 |
    d2 a4 |
    
    d2 c4 |
    b2 a4 |
    g g fis
    \time 2/2
  }
  \alternative { { g2 r4 d' } { g,1\fermata } }

  \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  My bon -- ny lass she smil -- eth,
  When she my heart be -- guil -- eth
  
  Fa la la la la la la.
  Fa la la la la la.
  Fa la la __ la la __ la la.
  la.
  
  \set stanza = #"1. "
  Smile less dear love there -- fore.
  And you shall love me more.
  
  Fa la la la la la.
  Fa la la la.
  Fa la la la la la la la la.
  \set stanza = #"1. "
  Smile
  la.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  When she her sweet eye turn -- eth,
  O how my heart it burn -- eth.
 
  "" \repeat unfold 20 \skip1
  
  \set stanza = #"2. "
  Dear love call in their light,
  Or else you’ll burn me quite.
  
  "" \repeat unfold 18 \skip1
  \set stanza = #"2. "
  Dear
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

altoMusic = \relative c'' {
  \repeat volta 2 {
    b4 b8 b b4 c |
    b2 b |
    b4 b8 b b4 a |
    
    a2 a |
    r1 |
    r1 |
    c4. c8 b4 b |
    a a g e
    
    %page2
    fis2. a4 |
    g2 e |
    d d |
    d2. c4
  } 
  \alternative { { b1 } { b2 r4 b' } } 
  \repeat volta 2 {
    c a g a |
    a2 r4 a |
    a a g e |
    \time 3/4 fis2 r4 |
    
    %page3
    r2. |
    g4. a8 b4 |
    c2 b4 |
    a2 g4 |
    fis2 e4 |
    
    d4. e8 fis4 |
    g4. f8 e4 |
    d e d |
    \time 2/2
  }
  \alternative { { d2 r4 b' } { d,1\fermata } }
  \bar "|."
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
quintusMusic = \relative c'' {
  \repeat volta 2 {
    g4 g8 g g4 g |
    g2 g |
    g4 g8 g d4 d |
    
    cis2 d |
    b4. b8 a4. a8 |
    g4 g a4. b8 |
    c4 c d d |
    d d g,2 |
    
    %page2
    d'2. e4 |
    d2 c~ |
    c b |
    a1 |
  }
  \alternative { { g1 } { g2 r4 g } }
  \repeat volta 2 {
    c d e a, |
    d2 r4 a |
    a a b a |
    \time 3/4 a2 r4 |
    
    %page3
    b2 a4 |
    g2 d'4 |
    g,2 g4 |
    a d g, |
    d'4 a8 b c4 |
    
    g8 a b4 a |
    g8 a b4 c |
    d c a |
    \time 2/2
  }
  \alternative { { b2 r4 g } { b1\fermata} } 
  \bar "|."
}
tenorMusic = \relative c' {
  \repeat volta 2 {
    d4 d8 d d4 c |
    d2 d |
    d4 d8 d b4 d |
    
    e2 fis |
    d4 e4. d8 d4~ |
    d cis d2 |
    g4. g8 g4 g |
    f d e2 |
    
    %page2
    a,2 a d, e |
    fis g |
    a d |
  }
  \alternative { { d1 }{ d2 r4 g } }
  \repeat volta 2 {
    g fis e e |
    fis2 r4 d |
    cis d e e |
    \time 3/4
    d2 r4 |
    
    %page3
    r4 g d |
    e2 b4 |
    e4. fis8 g4 |
    fis d e |
    a,2 r4 |
    
    r2 a4 |
    d2 c4 |
    b a a |
    \time 2/2
  }
  \alternative { { g2 r4 g' } { g,1\fermata}}
  \bar "|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat volta 2 {
    g4 g8 g g4 c, |
    g'2 g |
    g4 g8 g g4 d |
    
    a'2 d, |
    g4. g8 fis4. fis8 |
    e4 e d d |
    e c g' g, |
    d' f e2 |
    
    %page2
    d2. a4 |
    b2 c |
    d1~ |
    d |
  }
  \alternative { { g,1 } { g } } 
  \repeat volta 2 {
    r1 |
    r2 r4 d' |
    a' fis g a |
    \time 3/4
    d,2 r4 |
    
    %page3
    g2 fis4 |
    e2 d4 |
    c2 g4 |
    d'4 f e |
    d2 c4 |
    
    b g a |
    b g a |
    b c d |
    \time 2/2
  }
  \alternative { { g,2 r2 } { g1\fermata}}
}
bassWords = \lyricmode {
}


altusWordsA = \lyricmode {
  \set stanza = #"1. "
  My bon -- ny lass she smil -- eth,
  When she my heart be -- guil -- eth
  
  Fa la la la la la la la la.
  Fa la la la.
  Fa la la la.
  la.
  
  \set stanza = #"1. "
  Smile less dear love there -- fore.
  And you shall love me more.
  
  Fa la la la la la la la.
  Fa la la la la la la la la la la.
  \set stanza = #"1. "
  Smile
  la.
}

altusWordsB = \lyricmode {
  \set stanza = #"2. "
  When she her sweet eye turn -- eth,
  O how my heart it burn -- eth.
  
  "" \repeat unfold 17 \skip1
  
  \set stanza = #"2. "
  Dear love call in their light,
  Or else you’ll burn me quite.
  
  "" \repeat unfold 18 \skip1
  \set stanza = #"2. "
  Dear
}


quintusWordsA = \lyricmode {
  \set stanza = #"1. "
  My bon -- ny lass she smil -- eth,
  When she my heart be -- guil -- eth
  
  Fa la la la la la la.
  Fa la la la.
  Fa la la la la.
  Fa la la __ la la la.
  la.

  \set stanza = #"1. "
  Smile less dear love there -- fore.
  And you shall love me more.
  
  Fa la la.
  Fa la la la.
  Fa la la.
  Fa la la la la la la la la la la la la la la.
  \set stanza = #"1. "
  Smile
  la.
}

quintusWordsB = \lyricmode {
  \set stanza = #"2. "
  When she her sweet eye turn -- eth,
  O how my heart it burn -- eth.
  
  "" \repeat unfold 22 \skip1
  
  \set stanza = #"2. "
  Dear love call in their light,
  Or else you’ll burn me quite.
  
  "" \repeat unfold 24 \skip1
  \set stanza = #"2. "
  Dear
}

tenorWordsA = \lyricmode {
  \set stanza = #"1. "
  My bon -- ny lass she smil -- eth,
  When she my heart be -- guil -- eth
  
  Fa la la la __ la la.
  Fa la la la la la la la la.
  Fa la la la la la la.
  la.
  
  \set stanza = #"1. "
  Smile less dear love there -- fore.
  And you shall love me more.
  
  Fa la la la la la la la la la la.
  Fa la la la la la la.
  \set stanza = #"1. "
  Smile
  la.
}

tenorWordsB = \lyricmode {
  \set stanza = #"2. "
  When she her sweet eye turn -- eth,
  O how my heart it burn -- eth.
  
  "" \repeat unfold 22 \skip1
  
  \set stanza = #"2. "
  Dear love call in their light,
  Or else you’ll burn me quite.
  
  "" \repeat unfold 17 \skip1
  \set stanza = #"2. "
  Dear
}
bassusWordsA = \lyricmode {
  \set stanza = #"1. "
  My bon -- ny lass she smil -- eth,
  When she my heart be -- guil -- eth
  
  Fa la la la la la la.
  Fa la la la.
  Fa la la la la.
  Fa la la la __ la.
  la.
  
  \set stanza = #"1. "
  And you shall love me more.
  
  Fa la la.
  Fa la la la la la la.
  Fa la la la la la la la la la la.
  la.
}

bassusWordsB = \lyricmode {
  \set stanza = #"2. "
  When she her sweet eye turn -- eth,
  O how my heart it burn -- eth.
  
  "" \repeat unfold 21 \skip1
  
  \set stanza = #"2. "
  Or else you’ll burn me quite.
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \set Staff.instrument = "Cantus"
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "sopranos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "sopranosII"  \lyricsto "sopranos" \sopWordsII
    
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altus"  \lyricsto "altos" \altusWordsA
    \new Lyrics = "altusB"  \lyricsto "altos" \altusWordsB
    
   \new Staff = quintus <<
      \clef "treble_8"
      \new Voice = "quintus" { << \global \quintusMusic >> }
    >>
    \new Lyrics = "quintus"  \lyricsto "quintus" \quintusWordsA
    \new Lyrics = "quintusB"  \lyricsto "quintus" \quintusWordsB
    
    \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "tenor"  \lyricsto "tenors" \tenorWordsA
    \new Lyrics = "tenorB"  \lyricsto "tenors" \tenorWordsB
    
    \new Staff = bassus <<
      \clef bass
      \new Voice = "basses" { \global \bassMusic }
    >>
    \new Lyrics = "bassus"  \lyricsto "basses" \bassusWordsA
    \new Lyrics = "bassusB"  \lyricsto "basses" \bassusWordsB
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      %\override VerticalAxisGroup #'staff-affinity = #0
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

\score {
  \unfoldRepeats
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \set Staff.instrument = "Cantus"
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "sopranos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "sopranosII"  \lyricsto "sopranos" \sopWordsII
    
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altus"  \lyricsto "altos" \altusWordsA
    \new Lyrics = "altusB"  \lyricsto "altos" \altusWordsB
    
   \new Staff = quintus <<
      \clef "treble_8"
      \new Voice = "quintus" { << \global \quintusMusic >> }
    >>
    \new Lyrics = "quintus"  \lyricsto "quintus" \quintusWordsA
    \new Lyrics = "quintusB"  \lyricsto "quintus" \quintusWordsB
    
    \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "tenor"  \lyricsto "tenors" \tenorWordsA
    \new Lyrics = "tenorB"  \lyricsto "tenors" \tenorWordsB
    
    \new Staff = bassus <<
      \clef bass
      \new Voice = "basses" { \global \bassMusic }
    >>
    \new Lyrics = "bassus"  \lyricsto "basses" \bassusWordsA
    \new Lyrics = "bassusB"  \lyricsto "basses" \bassusWordsB
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 150
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}



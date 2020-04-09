\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"A Warrior Bold"}}
  poet = \markup\oldStyleNum"Edwin Thomas"
  composer = \markup\oldStyleNum"Stephen Adams (1841–1913)"
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
       (padding . 0)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #98
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
  \key c \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 
  \repeat volta 2 {
    g'4 |
    c8. b16 c4 e, |
    g8. fis16 g4 g |
    a8 f c4 a' |
    g4. b8\rest g4 |
    c8. b16 c4 g |
    a8. b16 c4 cis |
    
    d4. c8 b a |
    g8.[( fis16] g4) a |
    b8. g16 d4 e8[ fis] |
    g4 b\rest g |
    f8 g b4 a |
    g2 e4 |
    f8 g b4 a |
    
    g2 b8\rest f |
    e8. f16 g4. g8 |
    a8. b16 c4. c8 |
    c8. b16 c4 d |
    e2\fermata d4 |
    c8. b16 c4. a8 |
    
    g8. fis16 g4. e8 |
    f g b4 a |
    g2 gis4 |
    a8. b16 c4 a |
  }
  \alternative {
    {
      g8. e16 c4 e |
      f8 g a4 d |
      \partial 2 c2 |
    }
    {\break
      g8 e c4 b'8\rest e, |
    }
  }
  f a d4. c8 |
  b d f2~ |
  f4 b,8\rest g fis g |
  a2 a4 |
  b8[ a g] f8 d16 g8.\fermata |
  c,4 b'4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	In days of old, when knights were bold,
    And ba -- rons held their sway,
  A war -- rior bold, with spurs of gold,
    Sang mer -- ri -- ly his lay,
    Sang mer -- ri -- ly his lay:
  “My love is young and fair,
    My love hath gold -- en hair,
  And eyes so blue, and heart so true,
    That none with her com -- pare,
  So what care I, though death be nigh,
    I’ll live for love or die,
  So what care I, though death be nigh,
    I’ll live for love or die.”
    
  death be nigh,
    I’ve fought for love,
    I’ve fought for love,
    I’ve fought for love,
  For love, for love I die.”
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  So this brave knight, in ar -- mor bright,
    Went gay -- ly to the fray;
  He fought the fight, but ere the night,
    His soul had passed a -- way,
    His soul had passed a -- way.
  The plight -- ed ring he wore,
    Was crushed, and wet with gore,
  Yet ere he died, he brave -- ly cried,
    “I kept the vow I swore,
  So what care I, though death be nigh,
    I’ve fought for love and die,
  So what care I, though
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
  \repeat volta 2 {
    e4 |
    e8. e16 e4 c |
    e8. dis16 e4 e |
    f8 c c4 f |
    e4. s8 e4 |
    e8. e16 e4 e |
    f8. f16 f4 g |
    
    fis4. fis8 fis fis |
    e2 e4 |
    d8. d16 c4 c |
    b s d |
    d8 d g4 f |
    e2 c4 |
    d8 d f4 f |
    
    e2 s8 d |
    c8. d16 e4. e8 |
    f8. f16 f4. f8 |
    f8. f16 f4 a |
    gis2 e4 |
    f8. f16 f4. f8 |
    
    e8. dis16 e4. c8 |
    d d f4 f |
    e2 e4 |
    f8. f16 f4 f |
  }
  \alternative {
    {
      e8. c16 c4 c |
      d8 d f4 f |
      \partial 2 e2 |
    }
    {
      e8 c a4 s8 c |
    }
  }
    
  d f a4. a8 |
  g b b2~ |
  b4 s8 d, d d |
  f2 f4 |
  f( d8) d d16 b8. |
  c4 s4 \bar"|."
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
    \repeat volta 2 {
    c4 |
    g8. g16 g4 g |
    c8. c16 c4 c |
    c8 a a4 c |
    c4. s8 c4 |
    g8. g16 g4 c |
    c8. b16 a4 a |
    
    a4. a8 d c |
    b2 a4 |
    g8. b16 fis4 a |
    g s b |
    b8 b d4 b |
    c2 g4 |
    b8 b d4 b |
    
    c2 s8 b |
    c8. c16 c4. c8 |
    c8. b16 a4. a8 |
    a8. gis16 a4 b |
    b2 gis4 |
    a8. gis16 a4. c8 |
    
    c8. c16 c4. g8 |
    b b d4 b |
    c2 c4 |
    c8. b16 a4 c |
  }
  \alternative {
    {
      c8. g16 e4 g |
      b8 b b4 b |
      c2 |
    }
    {
      c8 g a4 s8 a |
    }
  }
    
  a d d4. d8 |
  d b d2~ |
  d4 s8 b b b |
  b2 b4 |
  d8[ c b] b g16 f8. |
  e4 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
    \repeat volta 2{
    c,4 |
    c8. c16 c4 c |
    c8. c16 c4 c |
    f8 f f4 f |
    c4. d8\rest c4 |
    c8. c16 c4 c |
    f8. f16 f4 e |
    
    d4. d8 d dis |
    e2 cis4 |
    d8. d16 d4 d |
    g, d'\rest g |
    g8 g g4 g |
    c,2 c4 |
    g'8 g g4 g |
    
    c,2 d8\rest g |
    c,8. c16 c4. c8 |
    f8. f16 f4. f8 |
    f8. f16 f4 f |
    e2\fermata e4 |
    f8. f16 f4. f8 |
    
    c8. c16 c4. c8 |
    g' g g4 g |
    c,2 c4 |
    f8. f16 f4 f |
  }
  \alternative {
    {
      c8. c16 c4 c |
      g'8 g g4 g |
      c,2 |
    }
    {
      c8 c a4 d8\rest a |
    }
  }
  
  d d f4. f8 |
  g g g2~ |
  g4 d8\rest g g g |
  g2 g4 |
  g4. g8 g,16 g8.\fermata |
  c4 d4\rest \bar"|."
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
}
\score {
  \unfoldRepeats
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
}



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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #94
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
  \key bes \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  \repeat volta 3 {
    f16[ f] |
    d'4 bes8 f ees d |
    c4 ees8 g4 g16[ g] |
    f8[ e] f g4 f8 |
    d4. b'4\rest f16[ f] |
    
    d'4 bes8 f ees d |
    c4 ees8 g4 g16[ g] |
    f8[ ees'] d c[ g] a |
    bes4. b4\rest d16[ d] |
    d4 d16[ d] d4 d8 |
    
    d4 d8 d4 d16 d |
    d4 a16[ a] bes4 c8 |
    d4. b4\rest f16[ f]
    d'4 bes16[ bes] f8[ ees] d |
    
    c4 ees8 g4 g16[ g] |
  }
  \alternative {
    {
      f8[ ees'] d c[ g] a |
      bes4. b4\rest
    }
    {
      \partial 2.
      f4 f8 f[ g] a |
      bes4. b4\rest \bar"|."
    }
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set ignoreMelismata = ##t
  They _ sailed a -- way in a gal -- lant bark,
  Roy __ _ Neal and his fair young bride;
  They had ven -- tured all in that bound -- ing ark,
  That __ _ danced o’er the sil -- _ v’ry tide;
  But their hearts were _ young and spi -- rits light,
  And they dashed the __ _ tears a -- way,
  As they watched the __ _ shore _ re -- cede from sight
  Of their own __ _ sweet Dub -- _ lin Bay.
  
  \dropLyricsXV
  sailed from Dub -- _ lin Bay.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
	\set ignoreMelismata = ##t
  Three _ days they sailed when a storm a -- rose,
  And the light -- _ ning swept the deep;
  When the thun -- der crash broke the short re -- pose
  Of the wea -- _ ry sail -- _ or’s sleep.
  Roy __ _ Neal he __ _ clasped his weep -- ing bride,
  And he kissed the __ _ tears a -- way,
  “Oh, _ love, ’twas a fear -- _ ful hour,” he cried,
  “When we left __ _ sweet Dub -- _ lin Bay.”
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
	\set ignoreMelismata = ##t
  On the crowd -- ed deck of that doom -- ed ship,
  Some _ fell in their mute de -- spair,
  But __ _ some more calm, with a ho -- lier lip,
  Sought the God __ _ of storm _ in prayer.
  “She has struck on a rock!” the sea -- men cried,
  In the depth of their wild dis -- may;
  And the ship went _ down with that fair young bride,
  That __ _
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 8
  \repeat volta 3 {
    d16[ d] |
    f4 f8 d c bes |
    c4 c8 ees4 ees16[ ees] |
    d8[ cis] d ees4 c8 |
    bes4. s4 d16[ d] |
    
    f4 d8 d c bes |
    c4 c8 ees4 ees16[ ees] |
    d8[ g] f ees4 ees8 |
    d4. s4 g16[ g] |
    fis4 fis16[ fis] a4 a8 |
    
    g4 g8 g4 g16 g |
    fis4 fis16[ fis] g4 g8 |
    fis4. s4 ees16[ ees] |
    d8[ f] f16[ f] d8[ c] bes |
    
    c4 c8 ees4 ees16[ ees]
  }
  \alternative {
    {
      \partial 2.
      d8[ g] f ees4 ees8 |
      d4. s4
    }
    {
      d4 d8 d[ ees] ees |
      d4. s4 \bar"|."
    }
  }
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
  \partial 8
  \repeat volta 3 {
    bes16[ bes] |
    bes4 d8 bes a bes |
    bes4 c8 c4 c16[ c] |
    bes8[ bes] bes8 a4 a8 |
    bes4. s4 bes16[ bes] |
    
    bes4 f8 bes a bes |
    bes4 c8 c4 c16[ c] |
    bes8[ bes] bes a4 c8 |
    bes4. s4 bes16[ bes] |
    a4 a16[ a] c4 c8 |
    
    bes4 bes8 bes4 bes16 bes |
    a4 d16[ d] d4 c8 |
    a4. s4 a16[ a] |
    bes4 d16[ d] bes8[ a] bes |
    
    bes4 c8 c4 c16[ c]
  }
  \alternative {
    {
      \partial 2.
      bes8[ c] bes a[ bes] c |
      bes4. s4
    }
    {
      bes4 bes8 bes4 f8 |
      f4. s4 \bar"|."
    }
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 8
  \repeat volta 3 {
    bes,16[ bes] |
    bes4 bes8 bes c d |
    ees4 ees8 c4 c16[ c] |
    f8[ f] f f4 f8 |
    bes,4. d4\rest bes16[ bes] |
    
    bes4 bes8 bes bes bes |
    ees4 ees8 c4 c16[ c] |
    f8[ f] f f4 f8 |
    bes,4. d4\rest g16[ g] |
    d4 d16[ d] fis4 fis8 |
    
    g4 g8 g4 g16 g |
    d4 d16[ d] g4 ees8 |
    d4. d4\rest f16[ f] |
    bes,4 bes16[ bes] bes8[ bes] bes |
    
    ees4 ees8 c4 c16[ c]
  }
  \alternative {
    {
      \partial 2.
      f4 f8 f4 f8 |
      bes,4. d4\rest
    }
    {
      f4 f8 f[ ees] c |
      bes4. d4\rest \bar"|."
    }
  }
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Dublin Bay"}}
  composer = \markup\oldStyleNum"George Barker (1812–1876)"
  poet = \markup\oldStyleNum"Mrs. Crawford"
  tagline = ""
}}




